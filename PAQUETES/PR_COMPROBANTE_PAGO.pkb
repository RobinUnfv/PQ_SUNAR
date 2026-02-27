CREATE OR REPLACE PACKAGE BODY FACTU.PR_COMPROBANTE_PAGO IS

  /*-----------------------------------------------------------------------------
  Nombre      : SET_XML_OUT
  Proposito   : Procedimiento que devuelve el XML de resultado del proceso
  Referencias :
  Parametros  :
                p_cNoCia           VARCHAR2   Código de compañia
                p_cNoCliente       VARCHAR2   Código del cliente
                p_cNoOrden         VARCHAR2   Número de pedido
                p_cNoFactu         VARCHAR2   Número de boleta o factura
                p_dFecha           DATE       Fecha de comprobante de pago
                p_cError           VARCHAR2   Resultado operación
                p_cMsgValida       CURSOR     Cursor de Mensajes de Validación
                p_cXMLError        XMLTYPE    Errores reportados previamente
  Retorno     : P_cXMLOut   CLOB  Mensaje de error

  Log de Cambios
    Fecha         Autor                      Descripción
    08/10/2024    Robinzon Santana           Creacion
    03/01/2026    Robinzon Santana           <R-01> Mostrar guia temporal
  ----------------------------------------------------------------------------*/
  PROCEDURE SET_XML_OUT(
            p_cNoCia IN FACTU.ARPFOE.NO_CIA%TYPE,
            p_cNoCliente IN FACTU.ARPFOE.NO_CLIENTE%TYPE,
            p_cNoOrden IN FACTU.ARPFOE.NO_ORDEN%TYPE,
            p_cNoGuia IN FACTU.ARPFFE.NO_GUIA%TYPE,
            p_cNoFactu IN FACTU.ARFAFE.NO_FACTU%TYPE,
            p_dFecha IN FACTU.ARFAFE.FECHA%TYPE,
            p_cError IN VARCHAR2,
            p_cXMLError IN XMLTYPE,
            P_cXMLOut OUT NOCOPY CLOB) IS
            
    cRetError VARCHAR2(5);
    l_xml_Out XMLTYPE;
    l_xml_Error XMLTYPE; 
    --
    cGuiaTemp FACTU.ARFAFE.GUIA_TEMP%TYPE;
    
  BEGIN
  
     IF p_cError = '0,OK' THEN
       cRetError := 'OK';
     ELSE
       cRetError := 'ERROR';
       ROLLBACK;
     END IF;
     
     --<I R-01>
     BEGIN
       SELECT GUIA_TEMP
       INTO cGuiaTemp
       FROM FACTU.ARFAFE
       WHERE NO_CIA = p_cNoCia
       AND NO_FACTU = p_cNoFactu;
     EXCEPTION
       WHEN OTHERS THEN
              cGuiaTemp := NULL;
     END;
     --<F R-01>
     
     --Enviar los resultados de la operación
     SELECT XMLELEMENT("P_CTAG",
        XMLELEMENT("noCia", p_cNoCia),
        XMLELEMENT("noCliente", p_cNoCliente),    
        XMLELEMENT("noOrden", p_cNoOrden),
        -- XMLELEMENT("noGuia", p_cNoGuia), -- <R-01>
        XMLELEMENT("noGuia", cGuiaTemp),
        XMLELEMENT("noFactu", p_cNoFactu),
        XMLELEMENT("fecha", p_dFecha),
        XMLELEMENT("resultado", cRetError),
        XMLELEMENT("resEmisionCp",
          XMLELEMENT("mensajeValidacionList", XMLCONCAT(p_cXMLError, l_xml_Error)) ) )
     INTO l_xml_Out
     FROM Dual;     
     --
     SELECT XMLSERIALIZE(CONTENT DELETEXML(l_xml_Out, '//*[.="" or contains(.,"?")]') INDENT)
     INTO p_cXMLOut
     FROM Dual;
     --
     p_cXMLOut := REPLACE(p_cXMLOut, 'P_CTAG', 'emisionComprobantePagoResponse');
  
  END SET_XML_OUT;
  
   /*-----------------------------------------------------------------------------
  Nombre      : SET_TDU_EMI_CP
  Proposito   :   PROCESO QUE NOS PERMITE OBTENER EL ARRAY DE EMISION COMPROBANTE DE PAGO
  Referencias : 
  Parametros  :
                p_cTag       VARCHAR2
                p_cXMLInput  XML (CLob) con los parámetros de entrada necesario para crear la emisión
  Retorno     : 
               p_tEmiCp      FACTU.TDU_TABLA_EMI_CP con resultado del proceso
  Log de Cambios
    Fecha         Autor                      Descripción
    11/10/2024   Robinzon Santana           Creacion
  ----------------------------------------------------------------------------*/
  PROCEDURE SET_TDU_EMI_CP(p_cTag IN VARCHAR2, 
                           p_cXmlInput IN CLOB,
                           p_tEmiCp OUT FACTU.TDU_TABLA_EMI_CP
                           ) IS
    CURSOR C_EMI_CP(cTag IN VARCHAR2) IS
      SELECT
         tipoDoc,
         tipoCliente,
         tipoCambio,
         indDoc,
         mDsctoGlobal
      FROM XMLTABLE(REPLACE('/*/arfafe','*',cTag) PASSING xmltype(p_cXmlInput)
        COLUMNS
         tipoDoc VARCHAR2(2) PATH 'tipoDoc',
         tipoCliente VARCHAR2(2) PATH 'tipoCliente',
         tipoCambio NUMBER(8,4) PATH 'tipoCambio',
         indDoc VARCHAR2(1) PATH 'indDoc',
         mDsctoGlobal NUMBER(11,3) PATH 'mDsctoGlobal'
         );
  BEGIN
    DBMS_OUTPUT.PUT_LINE('>>> ENTRO PR_COMPROBANTE_PAGO.SET_TDU_EMI_CP');  
    p_tEmiCp := FACTU.TDU_TABLA_EMI_CP();
    
    FOR i IN C_EMI_CP(p_cTag) LOOP
       p_tEmiCp.EXTEND;
       p_tEmiCp( p_tEmiCp.LAST ) := FACTU.OBJ_EMI_CP(i.tipoDoc, i.tipoCliente, i.tipoCambio, i.indDoc, i.mDsctoGlobal );
    END LOOP;
  
  END SET_TDU_EMI_CP;
  
 /*-----------------------------------------------------------------------------
  Nombre      : EMISION_COMPRO_PAGO
  Proposito   :  PROCESO PARA EMITIR UN COMPROBANTE PAGO(BOLETA O FACTURA)
  Referencias : 
  Parametros  :
                p_cXMLInput      XML (CLob) con los parámetros de entrada necesario para crear la emisión
  Retorno     : 
               p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      Descripción
     06/10/2024   Robinzon Santana           Creacion
  ----------------------------------------------------------------------------*/
  PROCEDURE EMISION_COMPRO_PAGO(p_cXmlInput IN CLOB, p_cXmlOutput IN OUT NOCOPY CLOB) IS
    
    cError VARCHAR2(2000);
    l_xml_Error0 XMLTYPE;
    kcTag CONSTANT VARCHAR2(25) := 'emisionComprobantePago';
    --
    tArpfoe FACTU.TDU_TABLA_ARPFOE;    
    tArpfol FACTU.TDU_TABLA_ARPFOL;
    --
    nCantArpfoe INTEGER;
    nCantArpfol INTEGER;
    --
    cNoOrden   FACTU.ARPFOE.NO_ORDEN%TYPE;
    cCentro    FACTU.ARPFOE.CENTRO%TYPE;
    cNoCliente FACTU.ARPFOE.NO_CLIENTE%TYPE;
    --
    cNoCia     FACTU.ARPFOE.NO_CIA%TYPE;
    nConsDesde FACTU.ARFACC.CONS_DESDE%TYPE;
    kDocPedido CONSTANT CHAR(1) := 'P';
    cSerie     FACTU.ARFACC.SERIE%TYPE;
    --
    tArfafe  FACTU.TDU_TABLA_ARFAFE;
                           
    cNoFactu FACTU.ARFAFE.NO_FACTU%TYPE;
    --
    tEmiCp FACTU.TDU_TABLA_EMI_CP;
    --
    cNoGuia FACTU.ARPFFE.NO_GUIA%TYPE;
    nNoDocu FACTU.ARPFFE.NO_DOCU%TYPE;
    --
    cTipoDoc FACTU.ARFACC.TIPO_DOC%TYPE;
  
  BEGIN
    DBMS_OUTPUT.PUT_LINE('>>>>> INICIAMOS EMISIÓN COMPROBANTE DE PAGO');    
    
    cError := '0,OK';
    l_xml_Error0 := NULL;
    
    FACTU.PR_PEDIDO.SET_TDU_ARPFOE(kcTag, p_cXmlInput, cError, tArpfoe,cNoOrden);
    
    -- PEDIDO
    IF cError = '0,OK' THEN
    
        BEGIN
          nCantArpfoe := tArpfoe.COUNT();
        EXCEPTION
          WHEN OTHERS THEN
             nCantArpfoe := 0;
        END;
        
        IF nCantArpfoe > 0 THEN
           
           FACTU.PR_PEDIDO.GUARDAR_ARPFOE(tArpfoe, cError, l_xml_Error0);
           
           IF cError = '0,OK' THEN
           
               cNoCia   := tArpfoe(nCantArpfoe).NO_CIA;
               cCentro  := tArpfoe(nCantArpfoe).CENTRO;
               cNoCliente := tArpfoe(nCantArpfoe).NO_CLIENTE;
           
               FACTU.PR_PEDIDO.SET_TDU_ARPFOL(kcTag, p_cXmlInput, cError, tArpfol,cNoOrden);
           
               IF cError = '0,OK' THEN
                  
                  BEGIN
                    nCantArpfol := tArpfol.COUNT();
                  EXCEPTION
                    WHEN OTHERS THEN
                      nCantArpfol := 0;
                  END;
                  
                  IF nCantArpfol > 0 THEN
                     
                     FACTU.PR_PEDIDO.GUARDAR_ARPFOL(tArpfol,cError, l_xml_Error0);
                     
                     IF cError = '0,OK' THEN
                         cSerie := SUBSTR(cNoOrden,1,3);
                         FACTU.PR_DOCUMENTO.ACTUALIZAR_CORRELATIVO(cNoCia,cCentro,kDocPedido, cSerie,  nConsDesde,cError, l_xml_Error0);                                                                      
                     END IF;
                     
                  END IF;
               
               END IF;           
           END IF;
                      
        END IF;        
                                
    END IF;
    
    -- GUIA FICTA
    IF cError = '0,OK' THEN
       FACTU.PR_GUIA.GUARDAR_GUIA_FICTA(cNoCia, cNoOrden, cNoGuia, nNoDocu, cError );
       DBMS_OUTPUT.PUT_LINE('cNoCia = '||cNoCia||' , cNoCliente = '||cNoCliente||' , cNoOrden = '||cNoOrden||' , cNoGuia = '||cNoGuia);
    END IF;
    DBMS_OUTPUT.PUT_LINE(cError);
    -- FACTURA O BOLETA
    IF cError = '0,OK' THEN
       
       FACTU.PR_COMPROBANTE_PAGO.SET_TDU_EMI_CP(kcTag, p_cXmlInput,tEmiCp);
       
       FOR i IN 1..tEmiCp.Count LOOP
          cTipoDoc := tEmiCp(i).tipoDoc;
       END LOOP;
       
       IF NVL(cTipoDoc,'X') IN('F','B') THEN
           FACTU.PR_FACTURA.GUARDAR(cNoCia, cNoOrden, cNoGuia, nNoDocu, tEmiCp, cNoFactu, cError );
       
           IF cError = '0,OK' THEN
           
              cTipoDoc := SUBSTR(cNoFactu,1,1);
              cSerie := SUBSTR(cNoFactu,1,4);

              FACTU.PR_DOCUMENTO.ACTUALIZAR_CORRELATIVO(cNoCia,cCentro,cTipoDoc, cSerie,  nConsDesde,cError, l_xml_Error0);
              -- ARCHIVO PLANO SUNAT           
              --FACTU.PR_FACTURA.CREAR_ARCHIVO_SFS(cNoCia, cTipoDoc, cNoFactu);
                                                                                  
           END IF;
       END IF;
       
    END IF;
     
    IF cError != '0,OK' THEN 
       ROLLBACK;
       FACTU.PR_PEDIDO.PUSH_ERROR('EMISION_COMPRO_PAGO', SUBSTR(cError, 1, INSTR(cError, ',') - 1), SUBSTR(cError, INSTR(cError, ',') + 1), NULL, NULL, l_xml_Error0);
    END IF;
      
    FACTU.PR_COMPROBANTE_PAGO.SET_XML_OUT(cNoCia, cNoCliente, cNoOrden, cNoGuia, cNoFactu, SYSDATE, cError, l_xml_Error0, p_cXmlOutput );
    
  EXCEPTION
    WHEN OTHERS THEN
       ROLLBACK;        
    
  END EMISION_COMPRO_PAGO;
  
   /*-----------------------------------------------------------------------------
  Nombre      : EMISION_BOLETA_FACTURA
  Proposito   : PROCESO QUE NOS PERMITE CREAR UNA FACTURA O BOLETA DE UN PEDIDO
  Referencias : 
  Parametros  :
                p_cNoCia     Codigo de compañia
               p_cNoOrden   Codigo de pedido 
  Retorno     : 
               p_cError     Resultado del proceso
  Log de Cambios
    Fecha         Autor                      Descripción
     02/04/2025   Robinzon Santana           Creacion
  ----------------------------------------------------------------------------*/
  PROCEDURE EMISION_BOLETA_FACTURA(
    p_cNoCia IN FACTU.ARPFOE.NO_CIA%TYPE,
    p_cNoOrden IN FACTU.ARPFOE.NO_ORDEN%TYPE,
    p_cNoCliente IN FACTU.ARFAFE.NO_CLIENTE%TYPE,
    p_cTipoDoc IN FACTU.ARFAFE.TIPO_DOC%TYPE,
   -- p_cTipoCliente IN FACTU.ARFAFE.TIPO_CLIENTE%TYPE,
    p_cError IN OUT VARCHAR2
    ) IS
    
    cCentro FACTU.ARPFOE.CENTRO%TYPE;
    nConsDesde FACTU.ARFACC.CONS_DESDE%TYPE;
    --
    cNoGuia FACTU.ARPFFE.NO_GUIA%TYPE;
    nNoDocu FACTU.ARPFFE.NO_DOCU%TYPE;
    --
    cNoFactu FACTU.ARFAFE.NO_FACTU%TYPE;
    --
    tEmiCp FACTU.TDU_TABLA_EMI_CP;
    --
    cTipoDoc FACTU.ARFACC.TIPO_DOC%TYPE;
    cSerie FACTU.ARFACC.SERIE%TYPE;
    --
    l_xml_Error0 XMLTYPE;
    --
    cNoCliente FACTU.ARPFOE.NO_CLIENTE%TYPE;
    cGuiaTemp  FACTU.ARPFOE.GUIA_TEMP%TYPE;
    cNewNoOrden FACTU.ARPFOE.NO_ORDEN%TYPE;
    --
    cTipoCliene ARCCMC.TIPO_CLIENTE%TYPE;
    cTipoDocumento ARCCMC.TIPO_DOCUMENTO%TYPE;
    
  BEGIN
    p_cError := '0,OK';
    l_xml_Error0 := NULL;
    
    BEGIN
        SELECT A.CENTRO, A.NO_CLIENTE, A.GUIA_TEMP
        INTO cCentro, cNoCliente, cGuiaTemp
        FROM FACTU.ARPFOE A
        WHERE A.NO_CIA = p_cNoCia
        AND A.NO_ORDEN = p_cNoOrden;
    EXCEPTION
       WHEN OTHERS THEN
         p_cError := '1,ERROR EL PEDIDO NO VALIDO.';
    END;
    
    IF p_cNoCliente != cNoCliente THEN
       cNoCliente := p_cNoCliente;
    END IF;
    
    BEGIN
      SELECT C.TIPO_CLIENTE, C.TIPO_DOCUMENTO
      INTO cTipoCliene, cTipoDocumento
      FROM CXC.ARCCMC C
      WHERE NO_CIA = p_cNoCia
      AND NO_CLIENTE = cNoCliente;
    EXCEPTION
       WHEN OTHERS THEN
         p_cError := '1,ERROR EL CLIENTE NO VALIDO';
    END;
    
    IF p_cTipoDoc = 'F' THEN
       
       IF NVL(cTipoCliene,'') = 'V' THEN
          p_cError := '1,ERROR NO SE PUEDE REALIZAR LA FACTURA CON CLIENTE VARIOS';
       END IF;
       
       IF cTipoDocumento != 'RUC' AND LENGTH(cNoCliente) != 11 THEN
          p_cError := '1,ERROR NO SE PUEDE REALIZAR LA FACTURA POR EL TIPO DE CLIENTE';
       END IF;
    
    END IF;
    
    -- PEDIDO
    IF p_cError = '0,OK' THEN
       FACTU.PR_PEDIDO.COPIA_PEDIDO(p_cNoCia,p_cNoOrden,cNoCliente,SYSDATE,cGuiaTemp,1,cCentro,'P','941',cNewNoOrden,p_cError );
    END IF;
    
    
    -- GUIA FICTA
    IF p_cError = '0,OK' THEN
       FACTU.PR_GUIA.GUARDAR_GUIA_FICTA(p_cNoCia, cNewNoOrden, cNoGuia, nNoDocu, p_cError );
     --  DBMS_OUTPUT.PUT_LINE('cNoCia = '||cNoCia||' , cNoCliente = '||cNoCliente||' , cNoOrden = '||cNoOrden||' , cNoGuia = '||cNoGuia);
    END IF;

    -- FACTURA O BOLETA
    IF p_cError = '0,OK' THEN
       
      -- FACTU.PR_COMPROBANTE_PAGO.SET_TDU_EMI_CP(kcTag, p_cXmlInput,tEmiCp);
       tEmiCp := FACTU.TDU_TABLA_EMI_CP();
       tEmiCp.EXTEND;
       tEmiCp( tEmiCp.LAST ) := FACTU.OBJ_EMI_CP(p_cTipoDoc, cTipoCliene, 3.37 , 'N', 0 );
       
       FACTU.PR_FACTURA.GUARDAR(p_cNoCia, cNewNoOrden, cNoGuia, nNoDocu, tEmiCp, cNoFactu, p_cError );
       
       IF p_cError = '0,OK' THEN
       
          cTipoDoc := SUBSTR(cNoFactu,1,1);
          cSerie := SUBSTR(cNoFactu,1,4);

          FACTU.PR_DOCUMENTO.ACTUALIZAR_CORRELATIVO(p_cNoCia,cCentro,cTipoDoc, cSerie,  nConsDesde,p_cError, l_xml_Error0);
                                                                              
       END IF;
       
    END IF;
     
    IF p_cError = '0,OK' THEN 
       p_cError := '0,'||cNoFactu;
    ELSE   
       ROLLBACK;
    END IF;

  END EMISION_BOLETA_FACTURA;
  
END PR_COMPROBANTE_PAGO;
/