CREATE OR REPLACE PACKAGE BODY FACTU.PR_PEDIDO IS
 
 /*-----------------------------------------------------------------------------
  Nombre      : DUPLICADO_PEDIDO
  Proposito   : REALIZAR EL DUPLICADO DEL PEDIDO
  Referencias :
  Parametros  :P_cNoCia       VARCHAR2   Numero de compañia
               P_cNoOrden     VARCHAR2   Numero pedido
               P_cNoCliente   VARCHAR2   Codigo del cliente
               P_dFecRegistro DATE       Codigo del cliente
               P_cGuiaTemp    VARCHAR2   Numero de guia
               P_cCentro      VARCHAR2    Centro de costo
               P_cTipoDoc     VARCHAR2    Tipo de documento
               P_sSerie       VARCHAR2    Serie del documento
                
  Retorno     : p_cError      VARCHAR2   Resultado operacion             

  Log de Cambios
    Fecha         Autor                      Descripcion
    28/01/2024   ROBINZON SANTANA           Creacion  
    03/04/2025   Robinzon Santana           Modificando fecha de creacion por la fecha actual 
  ----------------------------------------------------------------------------*/
  PROCEDURE DUPLICADO_PEDIDO(P_cNoCia IN VARCHAR2,
                             P_cNoOrden IN VARCHAR2, 
                             P_cNoCliente IN VARCHAR2, 
                             P_dFecRegistro IN FACTU.ARPFOE.FECHA_REGISTRO%TYPE,
                             P_cGuiaTemp IN VARCHAR,
                             P_nNumCopia IN NUMBER DEFAULT 1,
                             --
                             P_cCentro  IN FACTU.ARFACC.CENTRO%TYPE,
                             P_cTipoDoc IN FACTU.ARFACC.TIPO_DOC%TYPE DEFAULT 'P',
                             P_cSerie   IN FACTU.ARFACC.SERIE%TYPE,
                             --                             
                             P_cError IN OUT VARCHAR2
                             ) IS
  
  CURSOR C_ARPFOE IS
    SELECT NO_CIA,NO_ORDEN,GRUPO,NO_CLIENTE,DIVISION,NO_VENDEDOR, COD_T_PED,COD_FPAGO,
     F_RECEPCION,FECHA_REGISTRO,F_APROBACION,FECHA_ENTREGA,FECHA_ENTREGA_REAL,FECHA_VENCE,
     TIPO_PRECIO,MONEDA,SUB_TOTAL,T_IMPUESTO,T_PRECIO,IMPUESTO,ESTADO,BODEGA,CUSER,IGV,IND_GUIADO,
     DIRECCION_COMERCIAL,MOTIVO_TRASLADO,NOMBRE_CLIENTE,RUC,T_DESCUENTO,TIPO_DOC_REF,COD_CLAS_PED,
     TIPO_FPAGO,T_DSCTO_GLOBAL,T_VALOR_VENTA,COD_TIENDA,NOMB_TIENDA,DIREC_TIENDA,ALMA_ORIGEN,ALMA_DESTINO,
     TIPO_ARTI,TIPO_DOC_CLI, NUM_DOC_CLI,COD_DIR_ENTREGA,COD_DIR_SALIDA,NO_CLIENTE_SALIDA,ESTADO_ASIGNACION,
     LISTA_PREC_ANT,USUARIO_APROB,IND_VTA_ANTICIPADA,TOTAL_BRUTO,COD_T_PED1,COD_T_PEDB,COD_T_PEDN,TIPO,IND_PVENT,
     CENTRO,IND_FACTURA1,IND_BOLETA1,COD_CAJA,CAJERA,CONVENIO,CENTRO_COSTO,IND_NOTA_CRED,IND_EXPORTACION,CONSUMO,
     IND_FERIAS,IND_PROVINCIA,REDONDEO,IND_COD_BARRA,IND_FACT_TEXTO,IND_GUIA_TEXTO,FACTURA_TEXTO,IMPUESTO_FLETE,
     ON_LINE,CONT_NETO,IND_PROFORMA1,A_CTA,ENTREGA,HORA_ENTREGA,IND_PIDE_LOTE,MOT_CONTING,OPER_EXONERADAS,OPER_GRATUITAS,
     OPER_GRAVADAS,TIPO_OPERACION,GUIA_TEMP
    FROM FACTU.ARPFOE
    WHERE NO_CIA = P_cNoCia
    AND NO_ORDEN = P_cNoOrden;
    
  CURSOR C_ARPFOL IS
    SELECT NO_CIA,NO_ORDEN,GRUPO,NO_CLIENTE,NO_ARTI,TIPO_ARTI,ARTI_NUEVO,BODEGA,CANT_COMP,CANT_SOLICITADA,
      CANT_ENTREG,CANT_ASIGNADA,CANT_REASIGNADA,FECHA_REGISTRO,PRECIO,TOT_LINEA,ESTADO,DSCTO_CLIENTE,D_PROMO,
      IGV,NO_LINEA,P_DSCTO3,M_DSCTO2,M_DSCTO3,IMP_IGV,PRECIO_SIGV,TOTAL_LIN,DESCRIPCION,PARTE,TIPO_BS,IND_PIDE_LOTE,
      OPER_EXONERADAS,OPER_GRATUITAS,OPER_GRAVADAS,OPER_INAFECTAS,TIPO_AFECTACION,PREC_IGV,MEDIDA
    FROM FACTU.ARPFOL
    WHERE NO_CIA = P_cNoCia
    AND NO_ORDEN = P_cNoOrden;
    
  
  UNO C_ARPFOE%ROWTYPE;
  DOS C_ARPFOL%ROWTYPE;
  
  tduTabArpfoe TDU_TABLA_ARPFOE := TDU_TABLA_ARPFOE();
  
  i NUMBER(4);
  
  nConsDesde   FACTU.ARFACC.CONS_DESDE%TYPE;
  cNoCliente   FACTU.ARPFOE.NO_CLIENTE%TYPE;
  cFecRegistro FACTU.ARPFOE.FECHA_REGISTRO%TYPE;
  cNoOrden     FACTU.ARPFOE.NO_ORDEN%TYPE;
  cNombre      CXC.ARCCMC.NOMBRE%TYPE;
  cDireccion   CXC.ARCCTDA.DIRECCION%TYPE;
                             
  BEGIN

    IF P_cError = '0,OK' THEN
       
        nConsDesde := FACTU.PR_DOCUMENTO.OBTENER_CORRELATIVO(P_cNoCia,P_cCentro,P_cTipoDoc,P_cSerie);
        
        IF nConsDesde < 0 THEN
           P_cError := '1,El documento : '||P_cTipoDoc||' no tiene correlativo.';
           RETURN;
        END IF;
        
        nConsDesde := nConsDesde + 1;
        
        
       IF nConsDesde > 0 THEN
          cNoOrden := P_cSerie||LPAD( nConsDesde ,7,'0');
          dbms_output.put_line('El no_orden de arpfoe = '||cNoOrden);
          i := 1;
          LOOP
            EXIT WHEN i > P_nNumCopia;
            OPEN C_ARPFOE;
            LOOP
               UNO := NULL;
               
               FETCH C_ARPFOE INTO UNO;
               
               EXIT WHEN C_ARPFOE%NOTFOUND;
               dbms_output.put_line('SE VERIFICA EL CAMBIO DEL CLIENTE '||P_cNoCliente||' DEL PEDIDO.');
               IF NVL(P_cNoCliente,'X') = 'X' THEN
                  cNoCliente := UNO.NO_CLIENTE;
               ELSE
                 cNoCliente := P_cNoCliente;
               END IF;
               dbms_output.put_line('EL CLIENTE : '||cNoCliente||' DEL PEDIDO.');
               
               BEGIN
                 
                Select b.nombre, a.direccion
                INTO  cNombre, cDireccion
                From ARFATDIR c, ARCCTDA a, arccmc b
                Where a.no_cia     = P_cNoCia
                and   a.no_cliente = cNoCliente
                and a.cod_tienda = '001'
                and c.tipo       = 'LEG'
                and a.activo     = 'S'
                and a.no_cia     = c.no_cia
                and a.tipo_dir   = c.tipo
                and b.no_cia     = a.no_cia
                and b.no_cliente = a.no_cliente;
               
               EXCEPTION
                 WHEN OTHERS THEN
                    cDireccion := 'No Existe';
                    cNombre    := 'No Existe';
               END;
                
               dbms_output.put_line('VERIFICAMOS SI SE CAMBIO FECHA DE REGISTRO = '|| P_dFecRegistro ||' DEL PEDIDO');
               IF P_dFecRegistro IS NULL THEN
                   cFecRegistro := UNO.FECHA_REGISTRO;
               ELSE
                   cFecRegistro := P_dFecRegistro;
               END IF;
                               
              dbms_output.put_line('>>>>> INICIAMOS EL INSERT EN LA TABLA FACTU.ARPFOE');
              BEGIN
                 /*
                 tduTabArpfoe.EXTEND;
                 tduTabArpfoe(1) := OBJ_ARPFOE(UNO.NO_CIA,cNoOrden,UNO.GRUPO,cNoCliente,UNO.DIVISION,UNO.NO_VENDEDOR, UNO.COD_T_PED,UNO.COD_FPAGO,
                     cFecRegistro,cFecRegistro,cFecRegistro,cFecRegistro,cFecRegistro,cFecRegistro,
                     UNO.TIPO_PRECIO,UNO.MONEDA,UNO.SUB_TOTAL,UNO.T_IMPUESTO,UNO.T_PRECIO,UNO.IMPUESTO,'R',UNO.BODEGA,UNO.CUSER,UNO.IGV,UNO.IND_GUIADO,
                     cDireccion,UNO.MOTIVO_TRASLADO,cNombre,cNoCliente,UNO.T_DESCUENTO,UNO.TIPO_DOC_REF,UNO.COD_CLAS_PED,
                     UNO.TIPO_FPAGO,UNO.T_DSCTO_GLOBAL,UNO.T_VALOR_VENTA,UNO.COD_TIENDA,UNO.NOMB_TIENDA,UNO.DIREC_TIENDA,UNO.ALMA_ORIGEN,UNO.ALMA_DESTINO,
                     UNO.TIPO_ARTI,UNO.TIPO_DOC_CLI, UNO.NUM_DOC_CLI,UNO.COD_DIR_ENTREGA,UNO.COD_DIR_SALIDA,UNO.NO_CLIENTE_SALIDA,UNO.ESTADO_ASIGNACION,
                     UNO.LISTA_PREC_ANT,UNO.USUARIO_APROB,UNO.IND_VTA_ANTICIPADA,UNO.TOTAL_BRUTO,UNO.COD_T_PED1,UNO.COD_T_PEDB,UNO.COD_T_PEDN,UNO.TIPO,UNO.IND_PVENT,
                     UNO.CENTRO,UNO.IND_FACTURA1,UNO.IND_BOLETA1,UNO.COD_CAJA,UNO.CAJERA,UNO.CONVENIO,UNO.CENTRO_COSTO,UNO.IND_NOTA_CRED,UNO.IND_EXPORTACION,UNO.CONSUMO,
                     UNO.IND_FERIAS,UNO.IND_PROVINCIA,UNO.REDONDEO,UNO.IND_COD_BARRA,UNO.IND_FACT_TEXTO,UNO.IND_GUIA_TEXTO,UNO.FACTURA_TEXTO,UNO.IMPUESTO_FLETE,
                     UNO.ON_LINE,UNO.CONT_NETO,UNO.IND_PROFORMA1,UNO.A_CTA,cFecRegistro,UNO.HORA_ENTREGA,UNO.IND_PIDE_LOTE,UNO.MOT_CONTING,UNO.OPER_EXONERADAS,UNO.OPER_GRATUITAS,
                     UNO.OPER_GRAVADAS,UNO.TIPO_OPERACION,NVL(P_cGuiaTemp,''));
                     
                     FACTU.PR_PEDIDO.GUARDAR_ARPFOE(tduTabArpfoe, P_cError);
                     */
                
                INSERT INTO FACTU.ARPFOE( 
                     NO_CIA,NO_ORDEN,GRUPO,NO_CLIENTE,DIVISION,NO_VENDEDOR, COD_T_PED,COD_FPAGO,
                     F_RECEPCION,FECHA_REGISTRO,F_APROBACION,FECHA_ENTREGA,FECHA_ENTREGA_REAL,FECHA_VENCE,
                     TIPO_PRECIO,MONEDA,SUB_TOTAL,T_IMPUESTO,T_PRECIO,IMPUESTO,ESTADO,BODEGA,CUSER,IGV,IND_GUIADO,
                     DIRECCION_COMERCIAL,MOTIVO_TRASLADO,NOMBRE_CLIENTE,RUC,T_DESCUENTO,TIPO_DOC_REF,COD_CLAS_PED,
                     TIPO_FPAGO,T_DSCTO_GLOBAL,T_VALOR_VENTA,COD_TIENDA,NOMB_TIENDA,DIREC_TIENDA,ALMA_ORIGEN,ALMA_DESTINO,
                     TIPO_ARTI,TIPO_DOC_CLI, NUM_DOC_CLI,COD_DIR_ENTREGA,COD_DIR_SALIDA,NO_CLIENTE_SALIDA,ESTADO_ASIGNACION,
                     LISTA_PREC_ANT,USUARIO_APROB,IND_VTA_ANTICIPADA,TOTAL_BRUTO,COD_T_PED1,COD_T_PEDB,COD_T_PEDN,TIPO,IND_PVENT,
                     CENTRO,IND_FACTURA1,IND_BOLETA1,COD_CAJA,CAJERA,CONVENIO,CENTRO_COSTO,IND_NOTA_CRED,IND_EXPORTACION,CONSUMO,
                     IND_FERIAS,IND_PROVINCIA,REDONDEO,IND_COD_BARRA,IND_FACT_TEXTO,IND_GUIA_TEXTO,FACTURA_TEXTO,IMPUESTO_FLETE,
                     ON_LINE,CONT_NETO,IND_PROFORMA1,A_CTA,ENTREGA,HORA_ENTREGA,IND_PIDE_LOTE,MOT_CONTING,OPER_EXONERADAS,OPER_GRATUITAS,
                     OPER_GRAVADAS,TIPO_OPERACION,GUIA_TEMP )
                 VALUES(
                     UNO.NO_CIA,cNoOrden,UNO.GRUPO,cNoCliente,UNO.DIVISION,UNO.NO_VENDEDOR, UNO.COD_T_PED,UNO.COD_FPAGO,
                     cFecRegistro,cFecRegistro,cFecRegistro,cFecRegistro,cFecRegistro,cFecRegistro,
                     UNO.TIPO_PRECIO,UNO.MONEDA,UNO.SUB_TOTAL,UNO.T_IMPUESTO,UNO.T_PRECIO,UNO.IMPUESTO,'R',UNO.BODEGA,UNO.CUSER,UNO.IGV,UNO.IND_GUIADO,
                     cDireccion,UNO.MOTIVO_TRASLADO,cNombre,cNoCliente,UNO.T_DESCUENTO,UNO.TIPO_DOC_REF,UNO.COD_CLAS_PED,
                     UNO.TIPO_FPAGO,UNO.T_DSCTO_GLOBAL,UNO.T_VALOR_VENTA,UNO.COD_TIENDA,UNO.NOMB_TIENDA,UNO.DIREC_TIENDA,UNO.ALMA_ORIGEN,UNO.ALMA_DESTINO,
                     UNO.TIPO_ARTI,UNO.TIPO_DOC_CLI, UNO.NUM_DOC_CLI,UNO.COD_DIR_ENTREGA,UNO.COD_DIR_SALIDA,UNO.NO_CLIENTE_SALIDA,UNO.ESTADO_ASIGNACION,
                     UNO.LISTA_PREC_ANT,UNO.USUARIO_APROB,UNO.IND_VTA_ANTICIPADA,UNO.TOTAL_BRUTO,UNO.COD_T_PED1,UNO.COD_T_PEDB,UNO.COD_T_PEDN,UNO.TIPO,UNO.IND_PVENT,
                     UNO.CENTRO,UNO.IND_FACTURA1,UNO.IND_BOLETA1,UNO.COD_CAJA,UNO.CAJERA,UNO.CONVENIO,UNO.CENTRO_COSTO,UNO.IND_NOTA_CRED,UNO.IND_EXPORTACION,UNO.CONSUMO,
                     UNO.IND_FERIAS,UNO.IND_PROVINCIA,UNO.REDONDEO,UNO.IND_COD_BARRA,UNO.IND_FACT_TEXTO,UNO.IND_GUIA_TEXTO,UNO.FACTURA_TEXTO,UNO.IMPUESTO_FLETE,
                     UNO.ON_LINE,UNO.CONT_NETO,UNO.IND_PROFORMA1,UNO.A_CTA,cFecRegistro,UNO.HORA_ENTREGA,UNO.IND_PIDE_LOTE,UNO.MOT_CONTING,UNO.OPER_EXONERADAS,UNO.OPER_GRATUITAS,
                     UNO.OPER_GRAVADAS,UNO.TIPO_OPERACION,NVL(P_cGuiaTemp,'') );                     
                                                
              EXCEPTION
                WHEN OTHERS THEN
                   P_cError := '1,ERROR AL INSERTA EN ARPFOE :  '||SQLERRM;
                   RETURN;              
              END;
              
              OPEN C_ARPFOL;
              LOOP
                DOS := NULL;
                FETCH C_ARPFOL INTO DOS;
                EXIT WHEN C_ARPFOL%NOTFOUND;
                dbms_output.put_line('INICIAMOS INSERCION EN ARPFOL');
                BEGIN
                  
                  INSERT INTO FACTU.ARPFOL(
                      NO_CIA,NO_ORDEN,GRUPO,NO_CLIENTE,NO_ARTI,TIPO_ARTI,ARTI_NUEVO,BODEGA,CANT_COMP,CANT_SOLICITADA,
                      CANT_ENTREG,CANT_ASIGNADA,CANT_REASIGNADA,FECHA_REGISTRO,PRECIO,TOT_LINEA,ESTADO,DSCTO_CLIENTE,D_PROMO,
                      IGV,NO_LINEA,P_DSCTO3,M_DSCTO2,M_DSCTO3,IMP_IGV,PRECIO_SIGV,TOTAL_LIN,DESCRIPCION,PARTE,TIPO_BS,IND_PIDE_LOTE,
                      OPER_EXONERADAS,OPER_GRATUITAS,OPER_GRAVADAS,OPER_INAFECTAS,TIPO_AFECTACION,PREC_IGV,MEDIDA)
                  VALUES(
                      DOS.NO_CIA,cNoOrden,DOS.GRUPO,cNoCliente,DOS.NO_ARTI,DOS.TIPO_ARTI,DOS.ARTI_NUEVO,DOS.BODEGA,DOS.CANT_COMP,DOS.CANT_SOLICITADA,
                      DOS.CANT_ENTREG,DOS.CANT_ASIGNADA,DOS.CANT_REASIGNADA,cFecRegistro,DOS.PRECIO,DOS.TOT_LINEA,DOS.ESTADO,DOS.DSCTO_CLIENTE,DOS.D_PROMO,
                      DOS.IGV,DOS.NO_LINEA,DOS.P_DSCTO3,DOS.M_DSCTO2,DOS.M_DSCTO3,DOS.IMP_IGV,DOS.PRECIO_SIGV,DOS.TOTAL_LIN,DOS.DESCRIPCION,DOS.PARTE,DOS.TIPO_BS,DOS.IND_PIDE_LOTE,
                      DOS.OPER_EXONERADAS,DOS.OPER_GRATUITAS,DOS.OPER_GRAVADAS,DOS.OPER_INAFECTAS,DOS.TIPO_AFECTACION,DOS.PREC_IGV,DOS.MEDIDA);
                
                EXCEPTION
                 WHEN OTHERS THEN
                   P_cError := '1,ERROR AL INSERTA EN ARPFOL :  '||SQLERRM;
                   RETURN;
                END;
                
              END LOOP;
              CLOSE C_ARPFOL;
              
            
            END LOOP;
            CLOSE C_ARPFOE;
            i := i + 1;
            
          END LOOP;
                                        
       ELSE
         P_cError := '1,No se puedo obtener el numero de correlativo del pedido. P_cNoCia : '||P_cNoCia||' , P_cCentro : '||P_cCentro||' , P_cTipoDoc :  '||P_cTipoDoc||' , P_cSerie : '||P_cSerie;
         RETURN;
       END IF;
    
    END IF;
         
  END DUPLICADO_PEDIDO;
  
  PROCEDURE COPIA_PEDIDO(P_cNoCia IN VARCHAR2,
                         P_cNoOrden IN VARCHAR2, 
                         P_cNoCliente IN VARCHAR2, 
                         P_dFecRegistro IN FACTU.ARPFOE.FECHA_REGISTRO%TYPE,
                         P_cGuiaTemp IN VARCHAR,
                         P_nNumCopia IN NUMBER DEFAULT 1,
                         --
                         P_cCentro  IN FACTU.ARFACC.CENTRO%TYPE,
                         P_cTipoDoc IN FACTU.ARFACC.TIPO_DOC%TYPE DEFAULT 'P',
                         P_cSerie   IN FACTU.ARFACC.SERIE%TYPE,
                         --
                         P_cNewNoOrden OUT VARCHAR2,
                         P_cError IN OUT VARCHAR2                        
                        ) IS
  
    nConsDesde   FACTU.ARFACC.CONS_DESDE%TYPE;
    cNoOrden     FACTU.ARPFOE.NO_ORDEN%TYPE;
   -- cNoCliente   FACTU.ARPFOE.NO_CLIENTE%TYPE;
                        
  BEGIN
    P_cError := '0,OK';
    
    IF P_cNoCia IS NULL THEN
        P_cError := '1,Codigo de compañia no puede ser nula';
        RETURN;
    END IF;
    
    IF P_cNoOrden IS NULL THEN
        P_cError := '1,Numero de pedido no puede ser nula';
        RETURN;
    END IF;
    
    
    IF P_dFecRegistro IS NULL THEN
        P_cError := '1,Fecha de registro no puede ser nula';
        RETURN;
    END IF;
    
    IF P_cCentro IS NULL THEN
        P_cError := '1,Centro de costo no puede ser nula';
        RETURN;
    END IF;
    
    IF P_cTipoDoc IS NULL THEN
        P_cError := '1,Tipo de documento no puede ser nula';
        RETURN;
    END IF;
    
    IF P_cSerie IS NULL THEN
        P_cError := '1,Serie documento no puede ser nula';
        RETURN;
    END IF;
    
    dbms_output.put_line('INICIO COPIA_PEDIDO ');
    
    BEGIN
      
      FACTU.PR_PEDIDO.DUPLICADO_PEDIDO(P_cNoCia,
                       P_cNoOrden,
                       P_cNoCliente,
                       P_dFecRegistro,
                       P_cGuiaTemp,
                       P_nNumCopia,
                       P_cCentro,
                       P_cTipoDoc,
                       P_cSerie,
                       P_cError
                      );
      -- nConsDesde := OBT_CORRE_PEDIDO(P_cNoCia,P_cCentro,P_cTipoDoc,P_cSerie);
        /*
        SELECT CONS_DESDE + 1
        INTO nConsDesde
        FROM FACTU.ARFACC
        WHERE NO_CIA = P_cNoCia
        AND CENTRO   = P_cCentro
        AND TIPO_DOC = P_cTipoDoc
        AND SERIE    = P_cSerie;
        */
        nConsDesde := FACTU.PR_DOCUMENTO.OBTENER_CORRELATIVO(P_cNoCia,P_cCentro,P_cTipoDoc,P_cSerie);
        
        IF nConsDesde < 0 THEN
           P_cError := '1,El documento : '||P_cTipoDoc||' no tiene correlativo.';
           RETURN;
        END IF;
        
        nConsDesde := nConsDesde + 1;
        
        -- ACTUALIZAMOS EL CORRELATIVO DE LA SERIE
        dbms_output.put_line('ACTUALIZAMOS EL CORRELATIVO DE LA SERIE');
        
        UPDATE FACTU.ARFACC
        SET CONS_DESDE = nConsDesde
        WHERE NO_CIA = P_cNoCia
        AND CENTRO   = P_cCentro
        AND TIPO_DOC = P_cTipoDoc
        AND SERIE    = P_cSerie;
        
        cNoOrden := P_cSerie||LPAD(nConsDesde ,7,'0');
                          
        P_cNewNoOrden := cNoOrden;
        dbms_output.put_line('NUEVO NUMERO DEL PEDIDO = '||P_cNewNoOrden); 
        
    EXCEPTION
     WHEN OTHERS THEN
        P_cError := '1,Error en PR_PEDIDO.DUPLICADO_PEDIDO - (' || SQLCODE || '-' || SQLERRM || ')';
        RETURN;
    END;
    
  END COPIA_PEDIDO;
  
  /*-----------------------------------------------------------------------------
  Nombre      : SetXMLOut
  Proposito   : Procedimiento que devuelve el XML de resultado del proceso
  Referencias :
  Parametros  :
                p_cNoCia           VARCHAR2   Código de compañia
                p_cNoOrden         VARCHAR2   Número de pedido
                p_cNoCliente       VARCHAR2   Código del cliente
                p_cError           VARCHAR2   Resultado operación
                p_cMsgValida       CURSOR     Cursor de Mensajes de Validación
                p_cXMLError        XMLTYPE    Errores reportados previamente
  Retorno     : P_cXMLOut   CLOB  Mensaje de error

  Log de Cambios
    Fecha         Autor                      Descripción
    27/08/2024    Robinzon Santana           Creacion  
  ----------------------------------------------------------------------------*/
  
  PROCEDURE SetXMLOut(
            p_cNoCia IN FACTU.ARPFOE.NO_CIA%TYPE,
            p_cNoOrden IN FACTU.ARPFOE.NO_ORDEN%TYPE,
            p_cNoCliente IN FACTU.ARPFOE.NO_CLIENTE%TYPE,
            p_cError IN VARCHAR2,
            p_cXMLError IN XMLTYPE,
            P_cXMLOut OUT NOCOPY CLOB
          ) IS
          
    cRetError VARCHAR2(5);
    l_xml_Out XMLTYPE;
    l_xml_Error XMLTYPE;
    rMsg PR_PEDIDO.rMsgValidaRecordType;
    
  BEGIN
     
     IF p_cError = '0,OK' THEN
       cRetError := 'OK';
     ELSE
       cRetError := 'ERROR';
       ROLLBACK;
     END IF;
  
     --Enviar los resultados de la operación
     SELECT XMLELEMENT("P_CTAG",
        XMLELEMENT("noCia", p_cNoCia),        
        XMLELEMENT("noOrden", p_cNoOrden),
        XMLELEMENT("noCliente", p_cNoCliente),
        XMLELEMENT("resultado", cRetError),
        XMLELEMENT("resPedido",
          XMLELEMENT("mensajeValidacionList", XMLCONCAT(p_cXMLError, l_xml_Error)) ) )
     INTO l_xml_Out
     FROM Dual;
     
     --
     SELECT XMLSERIALIZE(CONTENT DELETEXML(l_xml_Out, '//*[.="" or contains(.,"?")]') INDENT)
     INTO p_cXMLOut
     FROM Dual;
     --
     p_cXMLOut := REPLACE(p_cXMLOut, 'P_CTAG', 'emisionPedidoResponse');
         
  END SetXMLOut;
      
  /*-----------------------------------------------------------------------------
  Nombre      : PushError
  Proposito   : Concatenar los errores encontrados en formato XML
  Referencias :
  Parametros  :
                p_cProceso       VARCHAR2   Proceso
                p_cCodError      VARCHAR2   Código del error
                p_cMsg           VARCHAR2   Mensaje de error
                p_dFecProc       DATE       Fecha del error
                p_cUser          VARCHAR2   Usuario
  Retorno     : p_cXMLError         XMLType    resultado del proceso

  Log de Cambios
    Fecha         Autor                      Descripción
    04/06/2024   Robinzon Santana           Creacion
  ----------------------------------------------------------------------------*/
  PROCEDURE PUSH_ERROR(
       p_cProceso IN VARCHAR2,
       p_cCodError IN VARCHAR2,
       p_cMsg IN VARCHAR2,
       p_dFecProc IN DATE,
       p_cUser IN VARCHAR2,
       --
       p_cXMLError IN OUT NOCOPY XMLTYPE ) IS
  BEGIN
    
    SELECT XMLCONCAT(p_cXMLError,
      XMLELEMENT("mensajeValidacion",
           XMLFOREST(
             p_cProceso AS "nomProceso",
             p_cCodError AS "codError",
             p_cMsg AS "msjError",
             NVL(p_dFecProc, SYSDATE) AS "fecProceso",                          
             p_cUser AS "usuario"
           )
       )
      )
    INTO p_cXMLError
    FROM DUAL;
    --
    DBMS_OUTPUT.Put_Line('>>> ERROR : ' || p_cProceso || '=> Cod : ' || p_cCodError || ' => ' || p_cMsg);
  
  END PUSH_ERROR;
    
  /*-----------------------------------------------------------------------------
  Nombre      : GUARDAR_ARPFOE
  Proposito   : Proceso para guardar y actualizar el ARPFOE
  Referencias :
  Parametros  :
                p_TDU_TABLA_ARPFOE    TDU_TABLA_ARPFOE
                
  Log de Cambios
    Fecha         Autor                      Descripción
    08/06/2024   Robinzon Santana           Creacion
    19/06/2025   Robinzon Santana           <MN-0001>Actualizando la cantidad
  ----------------------------------------------------------------------------*/
  PROCEDURE GUARDAR_ARPFOE(UNO IN TDU_TABLA_ARPFOE, p_cError IN OUT VARCHAR2, p_cXMLError IN OUT NOCOPY XMLTYPE)
  IS
    nCantidad NUMBER(2) := 0;
    cNoOrden FACTU.ARPFOE.NO_ORDEN%TYPE;
    kTipoDoc CONSTANT CHAR(1) := 'P';
    kEstadoSerie CONSTANT CHAR(1) := 'S';
  BEGIN
     p_cError := '0,OK';

    BEGIN
      nCantidad := UNO.COUNT();
    EXCEPTION
      WHEN OTHERS THEN
        nCantidad := 0;
    END;
    
    DBMS_OUTPUT.PUT_LINE('>>> TOTAL DE REGISTRO DE  TDU_TABLA_ARPFOE ES : '||nCantidad);
    
    IF nCantidad > 0 THEN
       /* <I MN-0001>
       BEGIN
              SELECT SERIE||LPAD(TO_CHAR(NVL(TO_NUMBER(cons_desde) + 1,0)),7,'0')
              INTO cNoOrden
              FROM FACTU.ARFACC
              WHERE NO_CIA = UNO(nCantidad).NO_CIA
              AND CENTRO   = UNO(nCantidad).CENTRO
              AND TIPO_DOC = kTipoDoc
              AND ACTIVO   = kEstadoSerie;
       EXCEPTION
              WHEN OTHERS THEN
                   p_cError := '1,No se puedo obtener el numero de pedido FACTU.ARPFOE';
                   PUSH_ERROR('GUARDAR_ARPFOE', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError);
       END;
       <F MN-0001>*/   
       
      -- FOR i IN 1..UNO.COUNT() LOOP -- <MN-0001> Robinzon Santana Llacza / 19-07-2025
         FOR i IN 1..nCantidad LOOP
           UPDATE FACTU.ARPFOE
           SET GRUPO = UNO(i).GRUPO,
           NO_CLIENTE = UNO(i).NO_CLIENTE,
           DIVISION = UNO(i).DIVISION,
           NO_VENDEDOR = UNO(i).NO_VENDEDOR,
           COD_T_PED = UNO(i).COD_T_PED,
           COD_FPAGO = UNO(i).COD_FPAGO,
           F_RECEPCION = UNO(i).F_RECEPCION,
           FECHA_REGISTRO = UNO(i).FECHA_REGISTRO,
           F_APROBACION = UNO(i).F_APROBACION,
           FECHA_ENTREGA = UNO(i).FECHA_ENTREGA,
           FECHA_ENTREGA_REAL = UNO(i).FECHA_ENTREGA_REAL,
           FECHA_VENCE = UNO(i).FECHA_VENCE,
           TIPO_PRECIO = UNO(i).TIPO_PRECIO ,
           MONEDA = UNO(i).MONEDA ,
           SUB_TOTAL = UNO(i).SUB_TOTAL ,
           T_IMPUESTO = UNO(i).T_IMPUESTO,
           T_PRECIO = UNO(i).T_PRECIO,
           IMPUESTO = UNO(i).IMPUESTO,
           ESTADO = UNO(i).ESTADO,
           BODEGA = UNO(i).BODEGA,
           CUSER = UNO(i).CUSER,
           IGV = UNO(i).IGV,
           IND_GUIADO = UNO(i).IND_GUIADO,
           DIRECCION_COMERCIAL = UNO(i).DIRECCION_COMERCIAL,
           MOTIVO_TRASLADO = UNO(i).MOTIVO_TRASLADO,
           NOMBRE_CLIENTE = UNO(i).NOMBRE_CLIENTE,
           RUC = UNO(i).RUC,
           T_DESCUENTO = UNO(i).T_DESCUENTO,
           TIPO_DOC_REF = UNO(i).TIPO_DOC_REF,
           COD_CLAS_PED = UNO(i).COD_CLAS_PED,
           TIPO_FPAGO = UNO(i).TIPO_FPAGO,
           T_DSCTO_GLOBAL = UNO(i).T_DSCTO_GLOBAL,
           T_VALOR_VENTA = UNO(i).T_VALOR_VENTA,
           COD_TIENDA = UNO(i).COD_TIENDA,
           NOMB_TIENDA = UNO(i).NOMB_TIENDA,
           DIREC_TIENDA = UNO(i).DIREC_TIENDA,
           ALMA_ORIGEN = UNO(i).ALMA_ORIGEN,
           ALMA_DESTINO = UNO(i).ALMA_DESTINO,
           TIPO_ARTI = UNO(i).TIPO_ARTI,
           TIPO_DOC_CLI = UNO(i).TIPO_DOC_CLI,
           NUM_DOC_CLI = UNO(i).NUM_DOC_CLI,
           COD_DIR_ENTREGA = UNO(i).COD_DIR_ENTREGA,
           COD_DIR_SALIDA = UNO(i).COD_DIR_SALIDA,
           NO_CLIENTE_SALIDA = UNO(i).NO_CLIENTE_SALIDA,
           ESTADO_ASIGNACION = UNO(i).ESTADO_ASIGNACION,
           LISTA_PREC_ANT = UNO(i).LISTA_PREC_ANT,
           USUARIO_APROB = UNO(i).USUARIO_APROB,
           IND_VTA_ANTICIPADA = UNO(i).IND_VTA_ANTICIPADA,
           TOTAL_BRUTO = UNO(i).TOTAL_BRUTO,
           COD_T_PED1 = UNO(i).COD_T_PED1,
           COD_T_PEDB = UNO(i).COD_T_PEDB,
           COD_T_PEDN = UNO(i).COD_T_PEDN,
           TIPO = UNO(i).TIPO,
           IND_PVENT = UNO(i).IND_PVENT,
           CENTRO = UNO(i).CENTRO,
           IND_FACTURA1 = UNO(i).IND_FACTURA1,
           IND_BOLETA1 = UNO(i).IND_BOLETA1,
           COD_CAJA = UNO(i).COD_CAJA,
           CAJERA = UNO(i).CAJERA,
           CONVENIO = UNO(i).CONVENIO,
           CENTRO_COSTO = UNO(i).CENTRO_COSTO,
           IND_NOTA_CRED = UNO(i).IND_NOTA_CRED,
           IND_EXPORTACION = UNO(i).IND_EXPORTACION,
           CONSUMO = UNO(i).CONSUMO,
           IND_FERIAS = UNO(i).IND_FERIAS,
           IND_PROVINCIA = UNO(i).IND_PROVINCIA,
           REDONDEO = UNO(i).REDONDEO,
           IND_COD_BARRA = UNO(i).IND_COD_BARRA,
           IND_FACT_TEXTO = UNO(i).IND_FACT_TEXTO,
           IND_GUIA_TEXTO = UNO(i).IND_GUIA_TEXTO,
           FACTURA_TEXTO = UNO(i).FACTURA_TEXTO,
           IMPUESTO_FLETE = UNO(i).IMPUESTO_FLETE,
           ON_LINE = UNO(i).ON_LINE,
           CONT_NETO = UNO(i).CONT_NETO,
           IND_PROFORMA1 = UNO(i).IND_PROFORMA1,
           A_CTA = UNO(i).A_CTA,
           ENTREGA = UNO(i).ENTREGA,
           HORA_ENTREGA = UNO(i).HORA_ENTREGA,
           IND_PIDE_LOTE = UNO(i).IND_PIDE_LOTE,
           MOT_CONTING = UNO(i).MOT_CONTING,
           OPER_EXONERADAS = UNO(i).OPER_EXONERADAS,
           OPER_GRATUITAS = UNO(i).OPER_GRATUITAS,
           OPER_GRAVADAS = UNO(i).OPER_GRAVADAS,
           TIPO_OPERACION = UNO(i).TIPO_OPERACION,
           GUIA_TEMP = UNO(i).GUIA_TEMP
           WHERE NO_CIA = UNO(i).NO_CIA
           AND NO_ORDEN = UNO(i).NO_ORDEN;
           
           DBMS_OUTPUT.PUT_LINE('>>> Total de registros actualizados en FACTU.ARPFOE : '||SQL%ROWCOUNT);
           
           IF SQL%ROWCOUNT = 0 THEN               
               
               BEGIN
                --  DBMS_OUTPUT.Put_Line('UNO(i).BODEGA = '|| UNO(i).BODEGA); <MN-0001> / 19-07-2025 / Robinzon Santana
                  
                  INSERT INTO FACTU.ARPFOE(
                             NO_CIA,NO_ORDEN,GRUPO,NO_CLIENTE,DIVISION,NO_VENDEDOR, COD_T_PED,COD_FPAGO,
                             F_RECEPCION,FECHA_REGISTRO,F_APROBACION,FECHA_ENTREGA,FECHA_ENTREGA_REAL,FECHA_VENCE,
                             TIPO_PRECIO,MONEDA,SUB_TOTAL,T_IMPUESTO,T_PRECIO,IMPUESTO,ESTADO,BODEGA,CUSER,IGV,IND_GUIADO,
                             DIRECCION_COMERCIAL,MOTIVO_TRASLADO,NOMBRE_CLIENTE,RUC,T_DESCUENTO,TIPO_DOC_REF,COD_CLAS_PED,
                             TIPO_FPAGO,T_DSCTO_GLOBAL,T_VALOR_VENTA,COD_TIENDA,NOMB_TIENDA,DIREC_TIENDA,ALMA_ORIGEN,ALMA_DESTINO,
                             TIPO_ARTI,TIPO_DOC_CLI, NUM_DOC_CLI,COD_DIR_ENTREGA,COD_DIR_SALIDA,NO_CLIENTE_SALIDA,ESTADO_ASIGNACION,
                             LISTA_PREC_ANT,USUARIO_APROB,IND_VTA_ANTICIPADA,TOTAL_BRUTO,COD_T_PED1,COD_T_PEDB,COD_T_PEDN,TIPO,IND_PVENT,
                             CENTRO,IND_FACTURA1,IND_BOLETA1,COD_CAJA,CAJERA,CONVENIO,CENTRO_COSTO,IND_NOTA_CRED,IND_EXPORTACION,CONSUMO,
                             IND_FERIAS,IND_PROVINCIA,REDONDEO,IND_COD_BARRA,IND_FACT_TEXTO,IND_GUIA_TEXTO,FACTURA_TEXTO,IMPUESTO_FLETE,
                             ON_LINE,CONT_NETO,IND_PROFORMA1,A_CTA,ENTREGA,HORA_ENTREGA,IND_PIDE_LOTE,MOT_CONTING,OPER_EXONERADAS,OPER_GRATUITAS,
                             OPER_GRAVADAS,TIPO_OPERACION,GUIA_TEMP )
                         VALUES(
                             UNO(i).NO_CIA, UNO(i).NO_ORDEN ,UNO(i).GRUPO, UNO(i).NO_CLIENTE,UNO(i).DIVISION,UNO(i).NO_VENDEDOR, UNO(i).COD_T_PED,UNO(i).COD_FPAGO,
                             UNO(i).F_RECEPCION, UNO(i).FECHA_REGISTRO, UNO(i).F_APROBACION, UNO(i).FECHA_ENTREGA, UNO(i).FECHA_ENTREGA_REAL, UNO(i).FECHA_VENCE,
                             UNO(i).TIPO_PRECIO,UNO(i).MONEDA,UNO(i).SUB_TOTAL,UNO(i).T_IMPUESTO,UNO(i).T_PRECIO,UNO(i).IMPUESTO,UNO(i).ESTADO,UNO(i).BODEGA,UNO(i).CUSER,UNO(i).IGV,UNO(i).IND_GUIADO,
                             UNO(i).DIRECCION_COMERCIAL,UNO(i).MOTIVO_TRASLADO, UNO(i).NOMBRE_CLIENTE, UNO(i).RUC ,UNO(i).T_DESCUENTO,UNO(i).TIPO_DOC_REF,UNO(i).COD_CLAS_PED,
                             UNO(i).TIPO_FPAGO, UNO(i).T_DSCTO_GLOBAL, UNO(i).T_VALOR_VENTA,UNO(i).COD_TIENDA,UNO(i).NOMB_TIENDA,UNO(i).DIREC_TIENDA,UNO(i).ALMA_ORIGEN,UNO(i).ALMA_DESTINO,
                             UNO(i).TIPO_ARTI,UNO(i).TIPO_DOC_CLI, UNO(i).NUM_DOC_CLI,UNO(i).COD_DIR_ENTREGA,UNO(i).COD_DIR_SALIDA,UNO(i).NO_CLIENTE_SALIDA,UNO(i).ESTADO_ASIGNACION,
                             UNO(i).LISTA_PREC_ANT,UNO(i).USUARIO_APROB,UNO(i).IND_VTA_ANTICIPADA,UNO(i).TOTAL_BRUTO,UNO(i).COD_T_PED1,UNO(i).COD_T_PEDB,UNO(i).COD_T_PEDN,UNO(i).TIPO,UNO(i).IND_PVENT,
                             UNO(i).CENTRO,UNO(i).IND_FACTURA1,UNO(i).IND_BOLETA1,UNO(i).COD_CAJA,UNO(i).CAJERA,UNO(i).CONVENIO,UNO(i).CENTRO_COSTO,UNO(i).IND_NOTA_CRED,UNO(i).IND_EXPORTACION,UNO(i).CONSUMO,
                             UNO(i).IND_FERIAS,UNO(i).IND_PROVINCIA,UNO(i).REDONDEO,UNO(i).IND_COD_BARRA,UNO(i).IND_FACT_TEXTO,UNO(i).IND_GUIA_TEXTO,UNO(i).FACTURA_TEXTO,UNO(i).IMPUESTO_FLETE,
                             UNO(i).ON_LINE,UNO(i).CONT_NETO,UNO(i).IND_PROFORMA1,UNO(i).A_CTA, UNO(i).ENTREGA,UNO(i).HORA_ENTREGA,UNO(i).IND_PIDE_LOTE,UNO(i).MOT_CONTING,UNO(i).OPER_EXONERADAS,UNO(i).OPER_GRATUITAS,
                             UNO(i).OPER_GRAVADAS,UNO(i).TIPO_OPERACION,UNO(i).GUIA_TEMP );
               EXCEPTION
                 WHEN OTHERS THEN
                    p_cError := '1,NO SE PUDO REGISTRAR LA CABECERA DEL PEDIDO. '||SQLERRM;
               END;
         
           END IF;
           
        END LOOP;
       
       dbms_OUTPUT.PUT_LINE('>>> SE TERMINO DE GUARDAR ARPFOE.');
       p_cError := '0,OK';
      
    ELSE
       p_cError := '1,No existe datos para guardar en FACTU.ARPFOE';
       PUSH_ERROR('GUARDAR_ARPFOE', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError);
    END IF;
    
  EXCEPTION
     WHEN OTHERS THEN
        p_cError := '1,ERROR AL INSERTA EN ARPFOE :  '||SQLERRM;
        PUSH_ERROR('GUARDAR_ARPFOE', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError);
        
  END GUARDAR_ARPFOE;
  
  /*-----------------------------------------------------------------------------
  Nombre      : CREAR_TDU_ARPFOE
  Proposito   : FUNCIÓN QUE DEVUELVE EL TDU CORRESPONDIENTE AL ARPFOE
  Referencias :
  Parametros  :
                p_cTag       VARCHAR2   Etiqueta XML principal del PEDIDO
                p_cXMLInput  XML (CLob) con los par?metros de entrada necesario para crear el PEDIDO

  Log de Cambios
    Fecha         Autor                      Descripción
    15/07/2024   Robinzon Santana            Creador
  ----------------------------------------------------------------------------*/
FUNCTION CREAR_TDU_ARPFOE(p_cTag IN VARCHAR2, p_cXmlInput IN CLOB) RETURN FACTU.TDU_TABLA_ARPFOE IS
    
    CURSOR C_ARPFOE(cTag IN VARCHAR2) IS
      SELECT noCia,
             noOrden,
             grupo,
             noCliente,
             division,
             noVendedor,
             codTped,
             codFpago,
             fRecepcion,
             fechaRegistro, -- 10
             fAprobacion,
             fechaEntrega,
             fechaEntregaReal,
             fechaVence,
             tipoPrecio,
             moneda,
             subTotal,
             tImpuesto,
             tPrecio,
             impuesto, -- 20
             estado,
             bodega,
             cuser,
             igv,
             indGuiado,
             direccionComercial,
             motivoTraslado,
             nombreCliente,
             ruc,
             tDescuento,-- 30
             tipoDocRef,
             codClasPed,
             tipoFpago,
             tDsctoGlobal,
             tValorVenta,
             codTienda,
             nombTienda,
             direcTienda,
             almaOrigen,
             almaDestino, -- 40
             tipoArti,
             tipoDocCli,
             numDocCli,
             codDirEntrega,
             codDirSalida,
             noClienteSalida,
             estadoAsignacion,
             listaPrecAnt,
             usuarioAprod,
             indVtaAnticipada, -- 50
             totalBruto,
             codTPed1,
             codTpedb,
             codTpedn,
             tipo,
             indPvent,
             centro,
             indFactura1,
             indBoleta1,
             codCaja, -- 60
             cajera,
             convenio,
             centroCosto,
             indNotaCred,
             indExportacion,
             consumo,
             indFerias,
             indProvincia,
             redondeo,
             indCodBarra, -- 70
             indFactTexto,
             indGuiaTexto,
             facturaTexto,
             impuestoFlete,
            -- onLine
             contNeto,
             indProforma1,
             aCta,
             entrega,
             horaEntrega, -- 80
             indPideLote,
             motConting,
             operExoneradas,
             operGratuitas,
             operGravadas,
             tipoOperacion,
             guiaTemp
             
      FROM XMLTABLE(REPLACE('/*/arpfoe','*',cTag) PASSING xmltype(p_cXmlInput)
        COLUMNS
             noCia VARCHAR2(2) PATH 'noCia',
             noOrden VARCHAR2(10) PATH 'noOrden',
             grupo VARCHAR2(2) PATH 'grupo',
             noCliente VARCHAR2(11) PATH 'noCliente',
             division VARCHAR2(3) PATH 'division',
             noVendedor VARCHAR2(10) PATH 'noVendedor',
             codTped VARCHAR2(4)  PATH 'codTped',
             codFpago VARCHAR2(2) PATH 'codFpago',
             fRecepcion DATE PATH 'fRecepcion',
             fechaRegistro DATE PATH 'fechaRegistro',
             fAprobacion DATE PATH 'fAprobacion',
             fechaEntrega DATE PATH 'fechaEntrega',
             fechaEntregaReal DATE PATH 'fechaEntregaReal',
             fechaVence DATE PATH 'fechaVence',
             tipoPrecio VARCHAR2(2) PATH 'tipoPrecio',
             moneda VARCHAR2(3) PATH 'moneda',
             subTotal NUMBER(11,3) PATH 'subTotal',
             tImpuesto NUMBER(11,3) PATH 'tImpuesto',
             tPrecio NUMBER(11,3) PATH 'tPrecio',
             impuesto NUMBER(11,3) PATH 'impuesto',
             estado VARCHAR2(1) PATH 'estado',
             bodega VARCHAR2(1) PATH 'bodega',
             cuser VARCHAR2(30) PATH 'cuser',
             igv NUMBER(5,3) PATH 'igv',
             indGuiado VARCHAR2(1) PATH 'indGuiado',
             direccionComercial VARCHAR2(120) PATH 'direccionComercial',
             motivoTraslado VARCHAR2(3) PATH 'motivoTraslado',
             nombreCliente VARCHAR2(80) PATH 'nombreCliente',
             ruc VARCHAR2(11) PATH 'ruc',
             tDescuento NUMBER(11,3) PATH 'tDescuento',
             tipoDocRef VARCHAR2(2) PATH 'tipoDocRef',
             codClasPed VARCHAR2(3) PATH 'codClasPed',
             tipoFpago VARCHAR2(2) PATH 'tipoFpago',
             tDsctoGlobal NUMBER(11,3) PATH 'tDsctoGlobal',
             tValorVenta NUMBER(11,3) PATH 'tValorVenta',
             codTienda VARCHAR2(2) PATH 'codTienda',
             nombTienda VARCHAR2(100) PATH 'nombTienda',
             direcTienda VARCHAR2(150) PATH 'direcTienda',
             almaOrigen VARCHAR2(5) PATH 'almaOrigen',
             almaDestino VARCHAR2(5) PATH 'almaDestino',
             tipoArti VARCHAR2(3) PATH 'tipoArti',
             tipoDocCli VARCHAR2(3) PATH 'tipoDocCli',
             numDocCli VARCHAR2(16) PATH 'numDocCli',
             codDirEntrega VARCHAR2(3) PATH 'codDirEntrega',
             codDirSalida VARCHAR2(3) PATH 'codDirSalida',
             noClienteSalida VARCHAR2(11) PATH 'noClienteSalida',
             estadoAsignacion VARCHAR2(1) PATH 'estadoAsignacion',
             listaPrecAnt VARCHAR2(1) PATH 'listaPrecAnt',
             usuarioAprod VARCHAR2(30) PATH 'usuarioAprod',
             indVtaAnticipada VARCHAR2(1) PATH 'indVtaAnticipada',
             totalBruto NUMBER(11,3) PATH 'totalBruto',
             codTPed1 VARCHAR2(4) PATH 'codTPed1',
             codTpedb VARCHAR2(4) PATH 'codTpedb',
             codTpedn VARCHAR2(4) PATH 'codTpedn',
             tipo VARCHAR2(1) PATH 'tipo',
             indPvent VARCHAR2(1) PATH 'indPvent',
             centro VARCHAR2(2) PATH 'centro',
             indFactura1 VARCHAR2(1) PATH 'indFactura1',
             indBoleta1 VARCHAR2(1) PATH 'indBoleta1',
             codCaja VARCHAR2(3) PATH 'codCaja',
             cajera VARCHAR2(6) PATH 'cajera',
             convenio VARCHAR2(1) PATH 'convenio',
             centroCosto VARCHAR2(4) PATH 'centroCosto',
             indNotaCred VARCHAR2(1) PATH 'indNotaCred',
             indExportacion VARCHAR2(1) PATH 'indExportacion',
             consumo VARCHAR2(1) PATH 'consumo',
             indFerias VARCHAR2(1) PATH 'indFerias',
             indProvincia VARCHAR2(1) PATH 'indProvincia',
             redondeo NUMBER(5,3) PATH 'redondeo',
             indCodBarra VARCHAR2(1) PATH 'indCodBarra', -- 70
             indFactTexto VARCHAR2(1) PATH 'indFactTexto',
             indGuiaTexto VARCHAR2(1) PATH 'indGuiaTexto',
             facturaTexto VARCHAR2(1) PATH 'facturaTexto',
             impuestoFlete NUMBER(11,3) PATH 'impuestoFlete',
             -- onLine VARCHAR2(1) PATH 'onLine'
             contNeto VARCHAR2(1) PATH 'contNeto',
             indProforma1 VARCHAR2(1) PATH 'indProforma1',
             aCta NUMBER(10,3) PATH 'aCta',
             entrega DATE PATH 'entrega',
             horaEntrega VARCHAR2(50) PATH 'horaEntrega', -- 80
             indPideLote VARCHAR2(1) PATH 'indPideLote',
             motConting CHAR(1) PATH 'motConting',
             operExoneradas NUMBER(11,2) PATH 'operExoneradas',
             operGratuitas NUMBER(11,2) PATH 'operGratuitas',
             operGravadas NUMBER(11,2) PATH 'operGravadas',
             tipoOperacion VARCHAR2(5) PATH 'tipoOperacion',
             guiaTemp VARCHAR2(25) PATH 'guiaTemp'
             );
             
    i NUMBER := 1;
    tTabla_Arpfoe FACTU.TDU_TABLA_ARPFOE := NULL;
            
  BEGIN
    
    DBMS_OUTPUT.PUT_LINE('>>>>> INICIAMOS LA FUNCION CREAR_TDU_APPFOE.');
    
    tTabla_Arpfoe := FACTU.TDU_TABLA_ARPFOE();
    
    FOR h IN C_ARPFOE(p_cTag) LOOP
      
      tTabla_Arpfoe.EXTEND(1);
      tTabla_Arpfoe(i) := FACTU.OBJ_ARPFOE(NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL, -- 10
                                     NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL, -- 20
                                     NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL, -- 30
                                     NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL, -- 40
                                     NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL, -- 50
                                     NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL, -- 60
                                     NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL, -- 70
                                     NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL, -- 80
                                     NULL,NULL,NULL,NULL,NULL,NULL,NULL); -- 87
                                     
      tTabla_Arpfoe(i).NO_CIA := h.noCia;
      tTabla_Arpfoe(i).NO_ORDEN := h.noOrden;
      tTabla_Arpfoe(i).GRUPO := h.grupo;
      tTabla_Arpfoe(i).NO_CLIENTE := h.noCliente;
      tTabla_Arpfoe(i).DIVISION := h.division;
      tTabla_Arpfoe(i).NO_VENDEDOR := h.noVendedor;
      tTabla_Arpfoe(i).COD_T_PED := h.codTped;
      tTabla_Arpfoe(i).COD_FPAGO := h.codFpago;
      tTabla_Arpfoe(i).F_RECEPCION := h.fRecepcion;
      tTabla_Arpfoe(i).FECHA_REGISTRO := h.fechaRegistro; -- 10
      tTabla_Arpfoe(i).F_APROBACION := h.fAprobacion;
      tTabla_Arpfoe(i).FECHA_ENTREGA := h.fechaEntrega;
      tTabla_Arpfoe(i).FECHA_ENTREGA_REAL := h.fechaEntregaReal;
      tTabla_Arpfoe(i).FECHA_VENCE := h.fechaVence;
      tTabla_Arpfoe(i).TIPO_PRECIO := h.tipoPrecio;
      tTabla_Arpfoe(i).MONEDA := h.moneda;
      tTabla_Arpfoe(i).SUB_TOTAL := h.subTotal;
      tTabla_Arpfoe(i).T_IMPUESTO := h.tImpuesto;
      tTabla_Arpfoe(i).T_PRECIO := h.tPrecio;
      tTabla_Arpfoe(i).IMPUESTO := h.impuesto; -- 20
      tTabla_Arpfoe(i).ESTADO := h.estado;
      tTabla_Arpfoe(i).BODEGA := h.bodega;
      tTabla_Arpfoe(i).CUSER := h.cuser;
      tTabla_Arpfoe(i).IGV := h.igv;
      tTabla_Arpfoe(i).IND_GUIADO := h.indGuiado;
      tTabla_Arpfoe(i).DIRECCION_COMERCIAL := h.direccionComercial;
      tTabla_Arpfoe(i).MOTIVO_TRASLADO := h.motivoTraslado;
      tTabla_Arpfoe(i).NOMBRE_CLIENTE := h.nombreCliente;
      tTabla_Arpfoe(i).RUC := h.ruc;
      tTabla_Arpfoe(i).T_DESCUENTO := h.tDescuento; -- 30
      tTabla_Arpfoe(i).TIPO_DOC_REF := h.tipoDocRef;
      tTabla_Arpfoe(i).COD_CLAS_PED := h.codClasPed;
      tTabla_Arpfoe(i).TIPO_FPAGO := h.tipoFpago;
      tTabla_Arpfoe(i).T_DSCTO_GLOBAL := h.tDsctoGlobal;
      tTabla_Arpfoe(i).T_VALOR_VENTA := h.tValorVenta;
      tTabla_Arpfoe(i).COD_TIENDA := h.codTienda;
      tTabla_Arpfoe(i).NOMB_TIENDA := h.nombTienda;
      tTabla_Arpfoe(i).DIREC_TIENDA := h.direcTienda;
      tTabla_Arpfoe(i).ALMA_ORIGEN := h.almaOrigen;
      tTabla_Arpfoe(i).ALMA_DESTINO := h.almaDestino; -- 40
      tTabla_Arpfoe(i).TIPO_ARTI := h.tipoArti;
      tTabla_Arpfoe(i).TIPO_DOC_CLI := h.tipoDocCli;
      tTabla_Arpfoe(i).NUM_DOC_CLI := h.numDocCli;
      tTabla_Arpfoe(i).COD_DIR_ENTREGA := h.codDirEntrega;
      tTabla_Arpfoe(i).COD_DIR_SALIDA := h.codDirSalida;
      tTabla_Arpfoe(i).NO_CLIENTE_SALIDA := h.noClienteSalida;
      tTabla_Arpfoe(i).ESTADO_ASIGNACION := h.estadoAsignacion;
      tTabla_Arpfoe(i).LISTA_PREC_ANT := h.listaPrecAnt;
      tTabla_Arpfoe(i).USUARIO_APROB := h.usuarioAprod;
      tTabla_Arpfoe(i).IND_VTA_ANTICIPADA := h.indVtaAnticipada; -- 50
      tTabla_Arpfoe(i).TOTAL_BRUTO := h.totalBruto;
      tTabla_Arpfoe(i).COD_T_PED1 := h.codTPed1;
      tTabla_Arpfoe(i).COD_T_PEDB := h.codTpedb;
      tTabla_Arpfoe(i).COD_T_PEDN := h.codTPedn;
      tTabla_Arpfoe(i).TIPO := h.tipo;
      tTabla_Arpfoe(i).IND_PVENT := h.indPvent;
      tTabla_Arpfoe(i).CENTRO := h.centro;
      tTabla_Arpfoe(i).IND_FACTURA1 := h.indFactura1;
      tTabla_Arpfoe(i).IND_BOLETA1 := h.indBoleta1;
      tTabla_Arpfoe(i).COD_CAJA := h.codCaja; -- 60
      tTabla_Arpfoe(i).CAJERA := h.cajera;
      tTabla_Arpfoe(i).CONVENIO := h.convenio;
      tTabla_Arpfoe(i).CENTRO_COSTO := h.centroCosto;
      tTabla_Arpfoe(i).IND_NOTA_CRED := h.indNotaCred;
      tTabla_Arpfoe(i).IND_EXPORTACION := h.indExportacion;
      tTabla_Arpfoe(i).CONSUMO := h.consumo;
      tTabla_Arpfoe(i).IND_FERIAS := h.indFerias;
      tTabla_Arpfoe(i).IND_PROVINCIA := h.indProvincia;
      tTabla_Arpfoe(i).REDONDEO := h.redondeo;
      tTabla_Arpfoe(i).IND_COD_BARRA := h.indCodBarra; -- 70
      tTabla_Arpfoe(i).IND_FACT_TEXTO := h.indFactTexto;
      tTabla_Arpfoe(i).IND_GUIA_TEXTO := h.indGuiaTexto;
      tTabla_Arpfoe(i).FACTURA_TEXTO := h.facturaTexto;
      tTabla_Arpfoe(i).IMPUESTO_FLETE := h.impuestoFlete;
     -- tTabla_Arpfoe(i).ON_LINE := h.onLine;
      tTabla_Arpfoe(i).CONT_NETO := h.contNeto;
      tTabla_Arpfoe(i).IND_PROFORMA1 := h.indProforma1;
      tTabla_Arpfoe(i).A_CTA := h.aCta;
      tTabla_Arpfoe(i).ENTREGA := h.entrega;
      tTabla_Arpfoe(i).HORA_ENTREGA := h.horaEntrega; -- 80
      tTabla_Arpfoe(i).IND_PIDE_LOTE := h.indPideLote;
      tTabla_Arpfoe(i).MOT_CONTING := h.motConting;
      tTabla_Arpfoe(i).OPER_EXONERADAS := h.operExoneradas;
      tTabla_Arpfoe(i).OPER_GRATUITAS := h.operGratuitas;
      tTabla_Arpfoe(i).OPER_GRAVADAS := h.operGravadas;
      tTabla_Arpfoe(i).TIPO_OPERACION := h.tipoOperacion;
      tTabla_Arpfoe(i).GUIA_TEMP := h.guiaTemp; -- 87
            
      i:= i + 1; 
    
    END LOOP;
    
    DBMS_OUTPUT.PUT_LINE('El arrego de arpfoe se cargo ...!!');
    
    RETURN tTabla_Arpfoe;
  
  END CREAR_TDU_ARPFOE;
 
/*-----------------------------------------------------------------------------
  Nombre      : PRE_VALIDACION
  Proposito   : Procedimiento que realiza validaciones preliminares comunes
  Referencias : 
  Parametros  :
               p_tdu_arpfoe         TABLA     FACTU.TDU_TABLA_ARPFOE
               
  Retorno     : p_cError           VARCHAR2   Resultado operación
                p_cXMLError        XMLTYPE    Errores reportados previamente

  Log de Cambios
    Fecha         Autor                      Descripción
    27/08/2024    Robinzon Santana           Creación   
  ----------------------------------------------------------------------------*/ 
  PROCEDURE PRE_VALIDACION( p_tdu_arpfoe FACTU.TDU_TABLA_ARPFOE, p_cError IN OUT VARCHAR2, p_cXMLError IN OUT NOCOPY XMLTYPE )
  IS
    cNoCia     FACTU.ARPFOE.NO_CIA%TYPE;
    cNoOrden   FACTU.ARPFOE.NO_ORDEN%TYPE;
    cNoCliente FACTU.ARPFOE.NO_CLIENTE%TYPE;
    dFechRegistro FACTU.ARPFOE.FECHA_REGISTRO%TYPE;
    cMoneda FACTU.ARPFOE.MONEDA%TYPE;
    nIgv FACTU.ARPFOE.IGV%TYPE;
    nTValorVenta FACTU.ARPFOE.T_VALOR_VENTA%TYPE;
    cCentro FACTU.ARPFOE.CENTRO%TYPE;
  BEGIN
    p_cError := '0,OK';
  
    DBMS_OUTPUT.PUT_LINE('>>> Entramos a FACTU.PR_PEDIDO.PRE_VALIDACION');
  
    FOR i IN 1..p_tdu_arpfoe.COUNT LOOP
    
       cNoCia := p_tdu_arpfoe(i).NO_CIA;
       cNoOrden := p_tdu_arpfoe(i).NO_ORDEN;
       cNoCliente := p_tdu_arpfoe(i).NO_CLIENTE;
       dFechRegistro := p_tdu_arpfoe(i).FECHA_REGISTRO;
       cMoneda := p_tdu_arpfoe(i).MONEDA;
       nIgv := p_tdu_arpfoe(i).IGV;
       nTValorVenta := p_tdu_arpfoe(i).T_VALOR_VENTA;
       cCentro := p_tdu_arpfoe(i).CENTRO;
       
       IF cNoCia IS NULL THEN
          p_cError := '1,Código de compañia no puede ser nulo';
          PUSH_ERROR('PRE_VALIDACION', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError);
       END IF;
       
       /* 04-10-2024 /  QUITAR VALIDACIÓN DEL NUMERO DE PEDIDO
       IF cNoOrden IS NULL THEN
          --raise_application_error(-20100,'Número de pedido no puede ser nulo.');
          p_cError := '1,Número de pedido no puede ser nulo.';
          PUSH_ERROR('PRE_VALIDACION', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError);
          
       ELSIF LENGTH(cNoOrden) < 10 OR 10 < LENGTH(cNoOrden) THEN
          p_cError := '1,Error en la longuitud ('|| LENGTH(cNoOrden) ||') del número del pedido. Solo se acepta diez caracteres numéricos';
          PUSH_ERROR('PRE_VALIDACION', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError);
       END IF;
       */
       
       IF cNoCliente IS NULL THEN
          -- raise_application_error(-20100,'Código de cliente no puede ser nulo.');
          p_cError := '1,Código de cliente no puede ser nulo.';
          PUSH_ERROR('PRE_VALIDACION', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError);
       END IF;
       
       IF dFechRegistro IS NULL THEN
          --raise_application_error(-20100,'Fecha de registro no puede ser nulo');
          p_cError := '1,Fecha de registro no puede ser nulo.';
          PUSH_ERROR('PRE_VALIDACION', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError);
       END IF;
       
       IF cMoneda IS NULL THEN
          -- raise_application_error(-20100,'Moneda no puede ser nulo.');
          p_cError := '1,Moneda no puede ser nulo.';
          PUSH_ERROR('PRE_VALIDACION', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError);
       END IF;
       
       IF nIgv IS NULL THEN
          --raise_application_error(-20100,'IGV no puede ser nulo.');
          p_cError := '1,IGV no puede ser nulo.';
          PUSH_ERROR('PRE_VALIDACION', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError);
       END IF;
       
       IF nTValorVenta IS NULL OR nTValorVenta <= 0 THEN
          -- raise_application_error(-20100,'Valor de venta no puede ser nulo o cero.');
          p_cError := '1,Valor de venta no puede ser nulo o cero.';
          PUSH_ERROR('PRE_VALIDACION', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError);
       END IF;
       
       IF cCentro IS NULL THEN
          --raise_application_error(-20100,'Centro no puede ser nulo.');
          p_cError := '1,El centro de costo no puede ser nulo.';
          PUSH_ERROR('PRE_VALIDACION', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError);
       END IF;
       
       -- VALIDAR CÓDIGO DE COMPAÑIA
       IF p_cError = '0,OK' THEN
          BEGIN
             SELECT NO_CIA
             INTO cNoCia
             FROM FACTU.ARFAMC
             WHERE NO_CIA = cNoCia;
          EXCEPTION
             WHEN OTHERS THEN
               p_cError := '1,Código de compañia no valido.';
               PUSH_ERROR('PRE_VALIDACION', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError); 
          END;
       END IF;
       
       -- VALIDAR CÓDIGO DE COMPÁÑIA
       IF p_cError = '0,OK' THEN
         BEGIN
           SELECT TRIM(NO_CLIENTE)
            INTO cNoCliente
            FROM CXC.ARCCMC
            WHERE NO_CIA = cNoCia
            AND NO_CLIENTE = cNoCliente;
         EXCEPTION
           WHEN OTHERS THEN
              p_cError := '1,Código del cliente '|| cNoCliente ||' no valido.';
              PUSH_ERROR('PRE_VALIDACION', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError);
         END;
       END IF;
       
       -- VALIDAR NÚMERO DE PEDIDO
       IF p_cError = '0,OK' THEN
         FACTU.PR_PEDIDO.VALIDA_NUM_PEDIDO(cNoCia, cCentro, cNoOrden, p_cError, p_cXMLError);
       END IF;
       
    END LOOP;
    
  END PRE_VALIDACION;
 
 /*---------------------------------------------------------------------------------------
   Nombre      : VALIDA_NUM_PEDIDO
   Proposito   : Validar número de pedido
   Parametro  :
               p_cNoCia    Número de pedido
               p_cCentro   Centro de venta
               p_cNoOrder  Numero de pedido

   Log de Cambios:
     Fecha        Autor                     Descripción
     24/08/2024   Robinzon Santana          Creador   
  -----------------------------------------------------------------------------------------*/
   PROCEDURE VALIDA_NUM_PEDIDO(p_cNoCia IN VARCHAR2, 
                               p_cCentro IN VARCHAR2, 
                               p_cNoOrder IN VARCHAR2,
                               p_cError IN OUT VARCHAR2, 
                               p_cXMLError IN OUT NOCOPY XMLTYPE ) IS
   
   cSerie FACTU.ARFACC.SERIE%TYPE;
   kTipoDoc CONSTANT CHAR(1) := 'P';
   kEstadoSerie CONSTANT CHAR(1) := 'S';
   cSerieEnviada FACTU.ARFACC.SERIE%TYPE;
   cNoOrdenActual FACTU.ARPFOE.NO_ORDEN%TYPE;
   
   BEGIN
     p_cError := '0,OK';
     DBMS_OUTPUT.PUT_LINE('>>> Entramos a FACTU.PR_PEDIDO.VALIDA_NUM_PEDIDO');
     
     IF p_cNoOrder IS NOT NULL THEN
        
        BEGIN
           SELECT 'S'
           INTO cSerie
           FROM FACTU.ARPFOE
           WHERE NO_CIA = p_cNoCia
           AND NO_ORDEN = p_cNoOrder;
        EXCEPTION
          WHEN OTHERS THEN
            cSerie := 'N';
        END;
        
        IF cSerie = 'N' THEN
           p_cError := '1,El numero de pedido no es válida. Numero de pedido enviado es : '||p_cNoOrder;
           PUSH_ERROR('VALIDA_NUM_PEDIDO', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError);
        END IF;
     
     END IF;
     
     /* 
     -- VALIDAMOS SERIE DEL PEDIDO
     BEGIN
        SELECT SERIE, SERIE||LPAD(TO_CHAR(NVL(TO_NUMBER(cons_desde),0)),7,'0')
        INTO cSerie,cNoOrdenActual
        FROM FACTU.ARFACC
        WHERE NO_CIA = p_cNoCia
        AND CENTRO = p_cCentro
        AND TIPO_DOC = kTipoDoc
        AND ACTIVO = kEstadoSerie;
     EXCEPTION
       WHEN OTHERS THEN
         cSerie := 'N';
     END;
     
     IF cSerie != 'N' THEN
         IF p_cNoOrder IS NOT NULL THEN
             cSerieEnviada := SUBSTR(p_cNoOrder,1,3);
             IF cSerie = cSerieEnviada THEN
                
                IF TO_NUMBER(cNoOrdenActual) >= TO_NUMBER(p_cNoOrder) THEN
                   p_cError := '1,El número del pedido enviado es mayor o igual al número del pedido actual. Número del pedido enviado es : '||p_cNoOrder;
                   PUSH_ERROR('VALIDA_NUM_PEDIDO', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError);
                END IF;
             
             ELSE
                p_cError := '1,La serie ingresada no es válida para el centro de costo enviado.';
                PUSH_ERROR('VALIDA_NUM_PEDIDO', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError);
             END IF;

         END IF;
     ELSE
        p_cError := '1,El centro de costo no tiene configurado SERIE en la tabla FACTU.ARFACC.';
        PUSH_ERROR('VALIDA_NUM_PEDIDO', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError);
     END IF;
     */
     
   END VALIDA_NUM_PEDIDO;
   
   
   /*-----------------------------------------------------------------------------
  Nombre      : CARGAR_TDU_TABLA_ARPFOL
  Proposito   : PROCEDIMIENTO PARA CARGAR DATOS DEL DETALLE DEL PEDIDO
  Referencias :
  Parametros  :
                p_cTag             VARCHAR2   Etiqueta XML principal
                p_cXMLInput        XML (CLob) con los parámetros de entrada necesario
                         
  Retorno     : p_tdu_arpfol       TABLA   ARRAY DE ARPFOL
                p_cError           VARCHAR2   Resultado de la operación

  Log de Cambios
    Fecha         Autor                      Descripción
    29/08/2024    Robinzon Santana           Creacion
  ----------------------------------------------------------------------------*/
  PROCEDURE CARGAR_TDU_TABLA_ARPFOL( p_cTag IN VARCHAR2, 
                                     p_cXmlInput IN CLOB,
                                     p_tdu_arpfol IN OUT NOCOPY FACTU.TDU_TABLA_ARPFOL,
                                     p_cError IN OUT VARCHAR2,
                                     p_cXMLError IN OUT NOCOPY XMLTYPE) IS
                                     
    cNoCia     FACTU.ARPFOL.NO_CIA%TYPE;
    cNoOrden   FACTU.ARPFOL.NO_ORDEN%TYPE;
                                 
    CURSOR C_ARPFOL(cTag IN VARCHAR2) IS
       SELECT noCia,
        noOrden,
        grupo,
        noCliente,
        noArti,
        tipoArti,
        artiNuevo,
        bodega,
        cantComp,
        cantSolicitada, -- 10
        cantEntreg,
        cantAsignada,
        cantReasignada,
        fechaRegistro,
        precio,
        totLinea,
        estado,
        dsctoCliente,
        dPromo,
        igv, -- 20
        noLinea,
        pDscto3,
        mDscto2,
        mDscto3,
        impIgv,
        precioSigv,
        totalLin,
        descripcion,
        parte,
        tipoBs, -- 30
        indPideLote,
        operExoneradas,
        operGratuitas,
        operGravadas,
        operInafectas,
        tipoAfectacion,
        precIgv,
        medida -- 38
       FROM XMLTABLE(REPLACE('/*/arpfolList/arpfol','*',cTag) PASSING xmltype(p_cXmlInput)
        COLUMNS
             noCia VARCHAR2(2) PATH 'noCia',
             noOrden VARCHAR2(10) PATH 'noOrden',
             grupo VARCHAR2(2) PATH 'grupo',
             noCliente VARCHAR2(11) PATH 'noCliente',
             noArti VARCHAR2(25) PATH 'noArti',
             tipoArti VARCHAR2(1) PATH 'tipoArti',
             artiNuevo VARCHAR2(1) PATH 'artiNuevo',
             bodega VARCHAR2(5) PATH 'bodega',
             cantComp NUMBER(12,3) PATH 'cantComp',
             cantSolicitada NUMBER(12,3) PATH 'cantSolicitada', -- 10
             cantEntreg NUMBER(12,3) PATH 'cantEntreg',
             cantAsignada NUMBER(12,3) PATH 'cantAsignada',
             cantReasignada NUMBER(12,3) PATH 'cantReasignada',
             fechaRegistro DATE PATH 'fechaRegistro',
             precio NUMBER(23,13) PATH 'precio',
             totLinea NUMBER(13,5) PATH 'totLinea' ,
             estado VARCHAR2(1) PATH 'estado',
             dsctoCliente NUMBER(12,3) PATH 'dsctoCliente',
             dPromo NUMBER(5,3) PATH 'dPromo',
             igv NUMBER(5,3) PATH 'igv', -- 20
             noLinea NUMBER(3) PATH 'noLinea',
             pDscto3 NUMBER(6,3) PATH 'pDscto3',
             mDscto2 NUMBER(12,3) PATH 'mDscto2',
             mDscto3 NUMBER(12,3) PATH 'mDscto3',
             impIgv NUMBER(12,5) PATH 'impIgv',
             precioSigv NUMBER(23,13) PATH 'precioSigv',
             totalLin NUMBER(12,5) PATH 'totalLin',
             descripcion VARCHAR2(500) PATH 'descripcion',
             parte NUMBER(3) PATH 'parte',
             tipoBs VARCHAR2(1) PATH 'tipoBs', -- 30
             indPideLote VARCHAR2(1) PATH 'indPideLote',
             operExoneradas NUMBER(12,2) PATH 'operExoneradas',
             operGratuitas NUMBER(12,2) PATH 'operGratuitas',
             operGravadas NUMBER(12,2) PATH 'operGravadas',
             operInafectas NUMBER(12,2) PATH 'operInafectas',
             tipoAfectacion VARCHAR2(2) PATH 'tipoAfectacion',
             precIgv NUMBER(18,2) PATH 'precIgv',
             medida VARCHAR2(10) PATH 'medida'  -- 38
             );
                           
  BEGIN
     p_cError := '0,OK';
     
     p_tdu_arpfol := FACTU.TDU_TABLA_ARPFOL();
     
     <<detalle>>
     FOR d IN C_ARPFOL(p_cTag) LOOP
     
       IF d.noCia IS NULL THEN
          p_cError := '1,En XML arpfol la etiqueta Código de compañia no puede ser nulo';
          PUSH_ERROR('CARGAR_TDU_TABLA_ARPFOL', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError);
       END IF;
       
       IF d.noOrden IS NULL THEN
          p_cError := '1,En XML arpfol la etiqueta Número de pedido no puede ser nulo.';
          PUSH_ERROR('CARGAR_TDU_TABLA_ARPFOL', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError);
       ELSIF LENGTH(d.noOrden) < 10 OR 10 < LENGTH(d.noOrden) THEN
          p_cError := '1,En XML arpfol la etiqueta Error en la longuitud ('|| LENGTH(d.noOrden) ||') del número del pedido. Solo se acepta diez caracteres numéricos';
          PUSH_ERROR('CARGAR_TDU_TABLA_ARPFOL', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError);
       END IF;
       
       IF d.noCliente IS NULL THEN
          p_cError := '1,En XML arpfol la etiqueta Código de cliente no puede ser nulo.';
          PUSH_ERROR('CARGAR_TDU_TABLA_ARPFOL', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError);
       END IF;
       
       IF d.noArti IS NULL THEN
          p_cError := '1,En XML arpfol la etiqueta Código de articulo no puede ser nulo.';
          PUSH_ERROR('CARGAR_TDU_TABLA_ARPFOL', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError);
       END IF;
              
     
       IF p_cError != '0,OK' THEN
           DBMS_OUTPUT.PUT_LINE('>>> Error en el XML => '||p_cError);
           EXIT detalle;
       ELSE
          p_tdu_arpfol.EXTEND;
          p_tdu_arpfol(p_tdu_arpfol.LAST) := OBJ_ARPFOL(
                                                d.noCia,
                                                d.noOrden,
                                                d.grupo,
                                                d.noCliente,
                                                d.noArti,
                                                d.tipoArti,
                                                d.artiNuevo,
                                                d.bodega,
                                                d.cantComp,
                                                d.cantSolicitada,
                                                d.cantEntreg,
                                                d.cantAsignada,
                                                d.cantReasignada,
                                                d.fechaRegistro,
                                                d.precio,
                                                d.totLinea,
                                                d.estado,
                                                d.dsctoCliente,
                                                d.dPromo,
                                                d.igv,
                                                d.noLinea,
                                                d.pDscto3,
                                                d.mDscto2,
                                                d.mDscto3,
                                                d.impIgv,
                                                d.precioSigv,
                                                d.totalLin,
                                                d.descripcion,
                                                d.parte,
                                                d.tipoBs,
                                                d.indPideLote,
                                                d.operExoneradas,
                                                d.operGratuitas,
                                                d.operGravadas,
                                                d.operInafectas,
                                                d.tipoAfectacion,
                                                d.precIgv,
                                                d.medida
                                                );
       END IF;
     
     END LOOP detalle;
          
  EXCEPTION
    WHEN OTHERS THEN
       p_cError := '1, FACTU.PR_PEDIDO.CARGAR_TDU_TABLA_ARPFOL ==> '||SQLERRM;
          
  END CARGAR_TDU_TABLA_ARPFOL;
  
  /*-----------------------------------------------------------------------------
  Nombre      : GUARDAR_ARPFOL
  Proposito   : PROCEDIMIENTO PARA GUARDAR EN LA TABLA FACTU.ARPFOL
  Referencias :
  Parametros  :
                TDU_TABLA_ARPFOL    TDU_TABLA_ARPFOE
                p_cError
                p_cXMLError
                
  Log de Cambios
    Fecha         Autor                      Descripción
    06/09/2024   Robinzon Santana           Creacion
    19/06/2025   Robinzon Santana           <MN-0001>Actualizando la cantidad
  ----------------------------------------------------------------------------*/
  PROCEDURE GUARDAR_ARPFOL(DET IN FACTU.TDU_TABLA_ARPFOL, p_cError IN OUT VARCHAR2, p_cXMLError IN OUT NOCOPY XMLTYPE)
  IS
    nCantidad NUMBER(2) := 0;
    
  BEGIN
  
    BEGIN
      nCantidad := DET.COUNT();
    EXCEPTION
      WHEN OTHERS THEN
         nCantidad := 0;
    END;
    
    IF nCantidad > 0 THEN
       
       FOR i IN 1..nCantidad LOOP
       
            UPDATE FACTU.ARPFOL
            SET NO_CIA = DET(i).NO_CIA,
                NO_ORDEN = DET(i).NO_ORDEN,
                GRUPO = DET(i).GRUPO,
                NO_CLIENTE = DET(i).NO_CLIENTE,
                NO_ARTI = DET(i).NO_ARTI,
                TIPO_ARTI = DET(i).TIPO_ARTI,
                ARTI_NUEVO = DET(i).ARTI_NUEVO,
                BODEGA = DET(i).BODEGA,
                CANT_COMP = DET(i).CANT_COMP,
                CANT_SOLICITADA = DET(i).CANT_SOLICITADA,
                CANT_ENTREG = DET(i).CANT_ENTREG,
                CANT_ASIGNADA = DET(i).CANT_ASIGNADA,
                CANT_REASIGNADA = DET(i).CANT_REASIGNADA,
                FECHA_REGISTRO = DET(i).FECHA_REGISTRO,
                PRECIO = DET(i).PRECIO,
                TOT_LINEA = DET(i).TOT_LINEA,
                ESTADO = DET(i).ESTADO,
                DSCTO_CLIENTE = DET(i).DSCTO_CLIENTE,
                D_PROMO = DET(i).D_PROMO,
                IGV = DET(i).IGV,
                NO_LINEA = DET(i).NO_LINEA,
                P_DSCTO3 = DET(i).P_DSCTO3,
                M_DSCTO2 = DET(i).M_DSCTO2,
                M_DSCTO3 = DET(i).M_DSCTO3,
                IMP_IGV = DET(i).IMP_IGV,
                PRECIO_SIGV = DET(i).PRECIO_SIGV,
                TOTAL_LIN = DET(i).TOTAL_LIN,
                DESCRIPCION = DET(i).DESCRIPCION,
                PARTE = DET(i).PARTE,
                TIPO_BS = DET(i).TIPO_BS,
                IND_PIDE_LOTE = DET(i).IND_PIDE_LOTE,
                OPER_EXONERADAS = DET(i).OPER_EXONERADAS,
                OPER_GRATUITAS = DET(i).OPER_GRATUITAS,
                OPER_GRAVADAS = DET(i).OPER_GRAVADAS,
                OPER_INAFECTAS = DET(i).OPER_INAFECTAS,
                TIPO_AFECTACION = DET(i).TIPO_AFECTACION,
                PREC_IGV = DET(i).PREC_IGV,
                MEDIDA = DET(i).MEDIDA
            WHERE NO_CIA = DET(i).NO_CIA
            AND NO_ORDEN = DET(i).NO_ORDEN
            AND NO_ARTI  = DET(i).NO_ARTI;
            
           -- DBMS_OUTPUT.PUT_LINE('>> Total de registros actualizados en FACTU.ARPFOL : '||SQL%ROWCOUNT); -- <MN-0001> / 19-07-2025 / Robinzon Santana
            
            IF SQL%ROWCOUNT = 0 THEN
               INSERT INTO FACTU.ARPFOL(
                      NO_CIA,NO_ORDEN,GRUPO,NO_CLIENTE,NO_ARTI,TIPO_ARTI,ARTI_NUEVO,BODEGA,CANT_COMP,CANT_SOLICITADA,
                      CANT_ENTREG,CANT_ASIGNADA,CANT_REASIGNADA,FECHA_REGISTRO,PRECIO,TOT_LINEA,ESTADO,DSCTO_CLIENTE,D_PROMO,
                      IGV,NO_LINEA,P_DSCTO3,M_DSCTO2,M_DSCTO3,IMP_IGV,PRECIO_SIGV,TOTAL_LIN,DESCRIPCION,PARTE,TIPO_BS,IND_PIDE_LOTE,
                      OPER_EXONERADAS,OPER_GRATUITAS,OPER_GRAVADAS,OPER_INAFECTAS,TIPO_AFECTACION,PREC_IGV,MEDIDA)
                  VALUES(DET(i).NO_CIA,DET(i).NO_ORDEN,DET(i).GRUPO,DET(i).NO_CLIENTE,DET(i).NO_ARTI,DET(i).TIPO_ARTI,DET(i).ARTI_NUEVO,DET(i).BODEGA,DET(i).CANT_COMP,DET(i).CANT_SOLICITADA,
                      DET(i).CANT_ENTREG,DET(i).CANT_ASIGNADA,DET(i).CANT_REASIGNADA,DET(i).FECHA_REGISTRO,DET(i).PRECIO,DET(i).TOT_LINEA,DET(i).ESTADO,DET(i).DSCTO_CLIENTE,DET(i).D_PROMO,
                      DET(i).IGV,DET(i).NO_LINEA,DET(i).P_DSCTO3,DET(i).M_DSCTO2,DET(i).M_DSCTO3,DET(i).IMP_IGV,DET(i).PRECIO_SIGV,DET(i).TOTAL_LIN,DET(i).DESCRIPCION,DET(i).PARTE,DET(i).TIPO_BS,DET(i).IND_PIDE_LOTE,
                      DET(i).OPER_EXONERADAS,DET(i).OPER_GRATUITAS,DET(i).OPER_GRAVADAS,DET(i).OPER_INAFECTAS,DET(i).TIPO_AFECTACION,DET(i).PREC_IGV,DET(i).MEDIDA
                      );               
            END IF;
             
       END LOOP;
       
       dbms_OUTPUT.PUT_LINE('>>> SE TERMINO DE GUARDAR FACTU.ARPFOL.');
       p_cError := '0,OK';
    
    END IF;
  
  EXCEPTION
     WHEN OTHERS THEN
        p_cError := '1,ERROR AL INSERTA EN FACTU.ARPFOL :  '||SQLERRM;
        PUSH_ERROR('GUARDAR_ARPFOL', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError);
        
  END GUARDAR_ARPFOL;
   
  /*---------------------------------------------------------------------------------------
   Nombre      : EMISION_PEDIDO
   Proposito   : Realizar la emsión del pedido
   Parametro   :p_cXMLInput      XML (CLob) con los parámetros de entrada necesario para crear la emisión
   Retorno:     p_cXMLOutput     XML (CLob) con resultado del proceso

   Log de Cambios:
     Fecha        Autor                     Descripción
     11/07/2024   Robinzon Santana          Creador
     19/07/2025   Robinzon Santana          <MN00002> Actualizacion del guardado de la cabecera del pedido 
  -----------------------------------------------------------------------------------------*/
  PROCEDURE EMISION_PEDIDO(p_cXmlInput IN CLOB, p_cXmlOutput IN OUT NOCOPY CLOB) IS
  
    kcTag CONSTANT VARCHAR2(20) := 'emisionPedido';
            
    cError VARCHAR2(2000);
    l_xml_Error0  XMLTYPE;
    
    tArpfoe FACTU.TDU_TABLA_ARPFOE;
    
    tArpfol FACTU.TDU_TABLA_ARPFOL;
    
    nCantArpfoe INTEGER;
    
    cNoCia     FACTU.ARPFOE.NO_CIA%TYPE;
    cNoOrden   FACTU.ARPFOE.NO_ORDEN%TYPE;
    cNoCliente FACTU.ARPFOE.NO_CLIENTE%TYPE;
    --
    cCentro FACTU.ARPFOE.CENTRO%TYPE;
    
    nConsDesde   FACTU.ARFACC.CONS_DESDE%TYPE;
    kTipoDoc CONSTANT CHAR(1) := 'P';
    cSerie   FACTU.ARFACC.SERIE%TYPE;
  
  BEGIN
  
    l_xml_Error0 := NULL;
    DBMS_OUTPUT.PUT_LINE('>>>>> INICIAMOS LA EMISIÓN DE PEDIDO');
    
    -- CARGAR DATOS DE LA CABECERA DEL PEDIDO
    tArpfoe := CREAR_TDU_ARPFOE(kcTag,p_cXmlInput);
    
    BEGIN
       nCantArpfoe := tArpfoe.COUNT;
    EXCEPTION
      WHEN OTHERS THEN
        nCantArpfoe := 0;
    END;
    
    IF nCantArpfoe > 0 THEN
    
       PRE_VALIDACION(tArpfoe, cError, l_xml_Error0);
       
       IF cError = '0,OK' THEN
         
          cNoCia := tArpfoe(nCantArpfoe).NO_CIA;
          cNoOrden := tArpfoe(nCantArpfoe).NO_ORDEN;
          cNoCliente := tArpfoe(nCantArpfoe).NO_CLIENTE;
          cCentro := tArpfoe(nCantArpfoe).CENTRO;
          
          GUARDAR_ARPFOE(tArpfoe, cError, l_xml_Error0);
          
          IF cError = '0,OK' THEN
            
            tArpfol := FACTU.TDU_TABLA_ARPFOL();
            
            CARGAR_TDU_TABLA_ARPFOL(kcTag, p_cXmlInput, tArpfol, cError, l_xml_Error0);
            
            IF cError != '0,OK' THEN
                PUSH_ERROR('EMISION_PEDIDO', SUBSTR(cError, 1, INSTR(cError, ',') - 1), SUBSTR(cError, INSTR(cError, ',') + 1), NULL, NULL, l_xml_Error0);
            ELSE
                IF tArpfol.COUNT > 0 THEN
                    DBMS_OUTPUT.PUT_LINE('----- TOTAL DE DETALLE : '||tArpfol.COUNT||' --------');
                    GUARDAR_ARPFOL(tArpfol,cError, l_xml_Error0);
                    
                    IF cError = '0,OK' THEN
                       cSerie := SUBSTR(cNoOrden,1,3);
                       FACTU.PR_DOCUMENTO.ACTUALIZAR_CORRELATIVO(cNoCia,cCentro,kTipoDoc, cSerie,  nConsDesde,cError, l_xml_Error0);
                       DBMS_OUTPUT.PUT_LINE('SE ACTUALIZO EL CORRELATIVO DE '||cSerie||'-'||nConsDesde);
                    END IF;
                    
                END IF;
            END IF;
            
          END IF;
         
       END IF;
                     
    ELSE
      cError := '1,No se cargo los datos para el array del pedido.';
      PUSH_ERROR('EMISION_PEDIDO', 'VAL-0049', cError, NULL, NULL, l_xml_Error0);
    END IF;
    
    -- ENVIAMOS LOS RESULTADOS DE LA EMISIÓN DEL PEDIDO
    SetXMLOut(cNoCia, cNoOrden, cNoCliente, cError, l_xml_Error0, p_cXmlOutput );
    
  END EMISION_PEDIDO;
  
  /*-----------------------------------------------------------------------------
  Nombre      : GUARDAR_PEDIDO
  Proposito   : PROCEDIMIENTO QUE VA ACTUALIZAR O INSERTAR UN PEDIDO
  Referencias :
  Parametros  :
              p_cXmlInput  XML(CLob) con parámatros necesarios de entrada para guardar pedido
 
  Retornos:
             p_cNoOrder   VARCHAR2    Número de pedido
             p_cError     VARCHAR2    Mensaje de error
                
  Log de Cambios
    Fecha         Autor                      Descripción
    05/10/2024   Robinzon Santana           Creacion
  ----------------------------------------------------------------------------*/
  PROCEDURE GUARDAR_PEDIDO(p_cXmlInput IN CLOB, 
                           p_cNoOrder OUT FACTU.ARPFOE.NO_ORDEN%TYPE,
                           p_cError OUT VARCHAR2) IS
                           
    cNewNoOrder FACTU.ARPFOE.NO_ORDEN%TYPE;
    cError      VARCHAR2(2000);
    
  BEGIN
  
    cError := '0,OK';
    
  END GUARDAR_PEDIDO;
  
   /*-----------------------------------------------------------------------------
  Nombre      : SET_TDU_ARPFOE
  Proposito   : PROCESO QUE NOS PERMITE OBTENER EL ARRAY DE ARPFOE
  Referencias : 
  Parametros  : 
                p_cTag      VARCHAR2 Tag principal 
                p_tArpfoe XML (CLob) con los parámetros de entrada
  Retorno     : 
               tArpfoe      FACTU.TDU_TABLA_ARPFOE   Array de ARPFOE
  Log de Cambios
    Fecha         Autor                      Descripción
     06/10/2024   Robinzon Santana           Creacion
     23/07/2025   Robinzon Santana           <MN-00003> MODIFICACION EN EL NUMERO DE PEDIDO
  ----------------------------------------------------------------------------*/
  PROCEDURE SET_TDU_ARPFOE(p_cTag IN VARCHAR2,
                      p_cXmlInput IN CLOB,
                      p_cError IN OUT VARCHAR2,
                      p_tArpfoe OUT FACTU.TDU_TABLA_ARPFOE,
                      p_cNoOrder OUT FACTU.ARPFOE.NO_ORDEN%TYPE) IS
         
    CURSOR C_ARPFOE(cTag IN VARCHAR2) IS
      SELECT noCia,
             noOrden,
             grupo,
             noCliente,
             division,
             noVendedor,
             codTped,
             codFpago,
             fRecepcion,
             fechaRegistro, -- 10
             fAprobacion,
             fechaEntrega,
             fechaEntregaReal,
             fechaVence,
             tipoPrecio,
             moneda,
             subTotal,
             tImpuesto,
             tPrecio,
             impuesto, -- 20
             estado,
             bodega,
             cuser,
             igv,
             indGuiado,
             direccionComercial,
             motivoTraslado,
             nombreCliente,
             ruc,
             tDescuento,-- 30
             tipoDocRef,
             codClasPed,
             tipoFpago,
             tDsctoGlobal,
             tValorVenta,
             codTienda,
             nombTienda,
             direcTienda,
             almaOrigen,
             almaDestino, -- 40
             tipoArti,
             tipoDocCli,
             numDocCli,
             codDirEntrega,
             codDirSalida,
             noClienteSalida,
             estadoAsignacion,
             listaPrecAnt,
             usuarioAprod,
             indVtaAnticipada, -- 50
             totalBruto,
             codTPed1,
             codTpedb,
             codTpedn,
             tipo,
             indPvent,
             centro,
             indFactura1,
             indBoleta1,
             codCaja, -- 60
             cajera,
             convenio,
             centroCosto,
             indNotaCred,
             indExportacion,
             consumo,
             indFerias,
             indProvincia,
             redondeo,
             indCodBarra, -- 70
             indFactTexto,
             indGuiaTexto,
             facturaTexto,
             impuestoFlete,
            -- onLine
             contNeto,
             indProforma1,
             aCta,
             entrega,
             horaEntrega, -- 80
             indPideLote,
             motConting,
             operExoneradas,
             operGratuitas,
             operGravadas,
             tipoOperacion,
             guiaTemp             
      FROM XMLTABLE(REPLACE('/*/arpfoe','*',cTag) PASSING xmltype(p_cXmlInput)
        COLUMNS
             noCia VARCHAR2(2) PATH 'noCia',
             noOrden VARCHAR2(10) PATH 'noOrden',
             grupo VARCHAR2(2) PATH 'grupo',
             noCliente VARCHAR2(11) PATH 'noCliente',
             division VARCHAR2(3) PATH 'division',
             noVendedor VARCHAR2(10) PATH 'noVendedor',
             codTped VARCHAR2(4)  PATH 'codTped',
             codFpago VARCHAR2(2) PATH 'codFpago',
             fRecepcion DATE PATH 'fRecepcion',
             fechaRegistro DATE PATH 'fechaRegistro',
             fAprobacion DATE PATH 'fAprobacion',
             fechaEntrega DATE PATH 'fechaEntrega',
             fechaEntregaReal DATE PATH 'fechaEntregaReal',
             fechaVence DATE PATH 'fechaVence',
             tipoPrecio VARCHAR2(2) PATH 'tipoPrecio',
             moneda VARCHAR2(3) PATH 'moneda',
             subTotal NUMBER(11,3) PATH 'subTotal',
             tImpuesto NUMBER(11,3) PATH 'tImpuesto',
             tPrecio NUMBER(11,3) PATH 'tPrecio',
             impuesto NUMBER(11,3) PATH 'impuesto',
             estado VARCHAR2(1) PATH 'estado',
             bodega VARCHAR2(5) PATH 'bodega',
             cuser VARCHAR2(30) PATH 'cuser',
             igv NUMBER(5,3) PATH 'igv',
             indGuiado VARCHAR2(1) PATH 'indGuiado',
             direccionComercial VARCHAR2(120) PATH 'direccionComercial',
             motivoTraslado VARCHAR2(3) PATH 'motivoTraslado',
             nombreCliente VARCHAR2(80) PATH 'nombreCliente',
             ruc VARCHAR2(11) PATH 'ruc',
             tDescuento NUMBER(11,3) PATH 'tDescuento',
             tipoDocRef VARCHAR2(2) PATH 'tipoDocRef',
             codClasPed VARCHAR2(3) PATH 'codClasPed',
             tipoFpago VARCHAR2(2) PATH 'tipoFpago',
             tDsctoGlobal NUMBER(11,3) PATH 'tDsctoGlobal',
             tValorVenta NUMBER(11,3) PATH 'tValorVenta',
             codTienda VARCHAR2(2) PATH 'codTienda',
             nombTienda VARCHAR2(100) PATH 'nombTienda',
             direcTienda VARCHAR2(150) PATH 'direcTienda',
             almaOrigen VARCHAR2(5) PATH 'almaOrigen',
             almaDestino VARCHAR2(5) PATH 'almaDestino',
             tipoArti VARCHAR2(3) PATH 'tipoArti',
             tipoDocCli VARCHAR2(3) PATH 'tipoDocCli',
             numDocCli VARCHAR2(16) PATH 'numDocCli',
             codDirEntrega VARCHAR2(3) PATH 'codDirEntrega',
             codDirSalida VARCHAR2(3) PATH 'codDirSalida',
             noClienteSalida VARCHAR2(11) PATH 'noClienteSalida',
             estadoAsignacion VARCHAR2(1) PATH 'estadoAsignacion',
             listaPrecAnt VARCHAR2(1) PATH 'listaPrecAnt',
             usuarioAprod VARCHAR2(30) PATH 'usuarioAprod',
             indVtaAnticipada VARCHAR2(1) PATH 'indVtaAnticipada',
             totalBruto NUMBER(11,3) PATH 'totalBruto',
             codTPed1 VARCHAR2(4) PATH 'codTPed1',
             codTpedb VARCHAR2(4) PATH 'codTpedb',
             codTpedn VARCHAR2(4) PATH 'codTpedn',
             tipo VARCHAR2(1) PATH 'tipo',
             indPvent VARCHAR2(1) PATH 'indPvent',
             centro VARCHAR2(2) PATH 'centro',
             indFactura1 VARCHAR2(1) PATH 'indFactura1',
             indBoleta1 VARCHAR2(1) PATH 'indBoleta1',
             codCaja VARCHAR2(3) PATH 'codCaja',
             cajera VARCHAR2(6) PATH 'cajera',
             convenio VARCHAR2(1) PATH 'convenio',
             centroCosto VARCHAR2(4) PATH 'centroCosto',
             indNotaCred VARCHAR2(1) PATH 'indNotaCred',
             indExportacion VARCHAR2(1) PATH 'indExportacion',
             consumo VARCHAR2(1) PATH 'consumo',
             indFerias VARCHAR2(1) PATH 'indFerias',
             indProvincia VARCHAR2(1) PATH 'indProvincia',
             redondeo NUMBER(5,3) PATH 'redondeo',
             indCodBarra VARCHAR2(1) PATH 'indCodBarra', -- 70
             indFactTexto VARCHAR2(1) PATH 'indFactTexto',
             indGuiaTexto VARCHAR2(1) PATH 'indGuiaTexto',
             facturaTexto VARCHAR2(1) PATH 'facturaTexto',
             impuestoFlete NUMBER(11,3) PATH 'impuestoFlete',
             -- onLine VARCHAR2(1) PATH 'onLine'
             contNeto VARCHAR2(1) PATH 'contNeto',
             indProforma1 VARCHAR2(1) PATH 'indProforma1',
             aCta NUMBER(10,3) PATH 'aCta',
             entrega DATE PATH 'entrega',
             horaEntrega VARCHAR2(50) PATH 'horaEntrega', -- 80
             indPideLote VARCHAR2(1) PATH 'indPideLote',
             motConting CHAR(1) PATH 'motConting',
             operExoneradas NUMBER(11,2) PATH 'operExoneradas',
             operGratuitas NUMBER(11,2) PATH 'operGratuitas',
             operGravadas NUMBER(11,2) PATH 'operGravadas',
             tipoOperacion VARCHAR2(5) PATH 'tipoOperacion',
             guiaTemp VARCHAR2(25) PATH 'guiaTemp'
             );
             
    
  cNoCia     FACTU.ARFAMC.NO_CIA%TYPE;
  cNoCliente CXC.ARCCMC.NO_CLIENTE%TYPE;
  cNoOrden   FACTU.ARPFOE.NO_ORDEN%TYPE;  
  
  BEGIN
    DBMS_OUTPUT.PUT_LINE('>>>>> ENTRAMOS A PR_COMPROBANTE_PAGO.SET_TDU_ARPFOE.');
    
    p_tArpfoe := FACTU.TDU_TABLA_ARPFOE();
    
    <<cabecera>>
    FOR h IN C_ARPFOE(p_cTag) LOOP
       
       IF h.noCia IS NULL THEN
          p_cError := '1,Código de compañia no puede ser nulo';
       END IF;
       
       IF h.noCliente IS NULL THEN
          p_cError := '1,Código de cliente no puede ser nulo.';
       END IF;
       
       IF h.fechaRegistro IS NULL THEN
          p_cError := '1,Fecha de registro no puede ser nulo.';
       END IF;
       
       IF h.moneda IS NULL THEN
          p_cError := '1,Moneda no puede ser nulo.';
       END IF;
       
       IF h.igv IS NULL THEN
          p_cError := '1,IGV no puede ser nulo.';
       END IF;
       
       IF NVL(h.tValorVenta,0) <= 0 THEN
          p_cError := '1,Valor de venta no puede ser nulo o cero.';
       END IF;
       
       IF h.centro IS NULL THEN
          p_cError := '1,El centro de costo no puede ser nulo.';
       END IF;
       
       -- VALIDAR CÓDIGO DE COMPAÑIA
       IF p_cError = '0,OK' THEN
          BEGIN
             SELECT NO_CIA
             INTO cNoCia
             FROM FACTU.ARFAMC
             WHERE NO_CIA = h.noCia;
          EXCEPTION
             WHEN OTHERS THEN
               p_cError := '1,Código de compañia no valido.';
          END;
       END IF;
       
       -- VALIDAR CÓDIGO DE COMPÁÑIA
       IF p_cError = '0,OK' THEN
         BEGIN
           SELECT TRIM(NO_CLIENTE)
            INTO cNoCliente
            FROM CXC.ARCCMC
            WHERE NO_CIA = h.noCia
            AND NO_CLIENTE = h.noCliente;
         EXCEPTION
           WHEN OTHERS THEN
              p_cError := '1,Código del cliente '|| h.noCliente ||' no valido.';
         END;
       END IF;
       
       --
       
       IF p_cError = '0,OK' THEN
           -- <I MN-00003>
           /*
           IF NVL(h.noOrden, 'X') = 'X' THEN
              p_cNoOrder := FACTU.PR_DOCUMENTO.GET_NUM_DOCUMENTO(h.noCia, h.centro, 'P', p_cError );
           ELSE
              p_cNoOrder := h.noOrden;
           END IF;
           */
           IF NVL(h.noOrden, 'X') = 'X' THEN
              cNoOrden := FACTU.PR_DOCUMENTO.GET_NUM_DOCUMENTO(h.noCia, h.centro, 'P', p_cError );
           ELSE
              cNoOrden := h.noOrden;
           END IF;
           p_cNoOrder := cNoOrden;
         
           -- <F MN-00003>  
       END IF;
       
       IF p_cError != '0,OK' THEN
           DBMS_OUTPUT.PUT_LINE('>>> Error en el XML => '||p_cError);
           EXIT cabecera;
       ELSE
         BEGIN
           p_tArpfoe.EXTEND;
           p_tArpfoe(p_tArpfoe.LAST) := FACTU.OBJ_ARPFOE(h.noCia, -- h.noOrden, <MN-00003>
                                     cNoOrden,h.grupo, h.noCliente, h.division, h.noVendedor, h.codTped, h.codFpago, h.fRecepcion, h.fechaRegistro, -- 10
                                     h.fAprobacion, h.fechaEntrega, h.fechaEntregaReal, h.fechaVence, h.tipoPrecio, h.moneda, h.subTotal, h.tImpuesto, h.tPrecio, h.impuesto, -- 20
                                     h.estado, h.bodega, h.cuser, h.igv, h.indGuiado, h.direccionComercial, h.motivoTraslado, h.nombreCliente, h.ruc, h.tDescuento, -- 30
                                     h.tipoDocRef, h.codClasPed, h.tipoFpago, h.tDsctoGlobal, h.tValorVenta, h.codTienda, h.nombTienda, h.direcTienda, h.almaOrigen, h.almaDestino, -- 40
                                     h.tipoArti, h.tipoDocCli, h.numDocCli, h.codDirEntrega, h.codDirSalida, h.noClienteSalida, h.estadoAsignacion, h.listaPrecAnt, h.usuarioAprod, h.indVtaAnticipada, -- 50
                                     h.totalBruto, h.codTPed1, h.codTpedb, h.codTPedn, h.tipo, h.indPvent, h.centro, h.indFactura1, h.indBoleta1, h.codCaja, -- 60
                                     h.cajera, h.convenio, h.centroCosto, h.indNotaCred, h.indExportacion, h.consumo, h.indFerias, h.indProvincia, h.redondeo, h.indCodBarra, -- 70
                                     h.indFactTexto, h.indGuiaTexto, h.facturaTexto, h.impuestoFlete, NULL , h.contNeto, h.indProforma1, h.aCta, h.entrega, h.horaEntrega, -- 80
                                     h.indPideLote, h.motConting, h.operExoneradas, h.operGratuitas, h.operGravadas, h.tipoOperacion, h.guiaTemp);
         EXCEPTION
             WHEN VALUE_ERROR THEN
                 p_cError := '1,Error SET_TDU_ARPFOE -> VALUE_ERROR: ' || SQLERRM;
                DBMS_OUTPUT.PUT_LINE('Error0 VALUE_ERROR: ' || SQLERRM);
             WHEN OTHERS THEN
                 p_cError := '1,Error SET_TDU_ARPFOE -> desconocido: ' || SQLERRM;
                DBMS_OUTPUT.PUT_LINE('Error0 desconocido: ' || SQLERRM);
         END;
       END IF;    
       
    END LOOP;
    
  EXCEPTION
     WHEN VALUE_ERROR THEN
        DBMS_OUTPUT.PUT_LINE('Error1 VALUE_ERROR: ' || SQLERRM);
        p_cError := '1,Error1 SET_TDU_ARPFOE -> VALUE_ERROR: ' || SQLERRM;
     WHEN OTHERS THEN
        p_cError := '1,Error1 SET_TDU_ARPFOE -> desconocido: ' || SQLERRM;
        DBMS_OUTPUT.PUT_LINE('Error1 desconocido: ' || SQLERRM);
  END SET_TDU_ARPFOE;
  
  /*-----------------------------------------------------------------------------
  Nombre      : SET_TDU_ARPFOL
  Proposito   :  PROCESO QUE NOS PERMITE OBTENER EL ARRAY DE ARPFOL
  Referencias : 
  Parametros  : 
                p_cTag      VARCHAR2    Tag principal 
                p_tArpfol   XML         (CLob) con los parámetros de entrada
  Retorno     : 
               tArpfoe      FACTU.TDU_TABLA_ARPFOL   Array de ARPFOE
  Log de Cambios
    Fecha         Autor                      Descripción
     07/10/2024   Robinzon Santana           Creacion
  ----------------------------------------------------------------------------*/
  PROCEDURE SET_TDU_ARPFOL(p_cTag IN VARCHAR2,
                      p_cXmlInput IN CLOB,
                      p_cError IN OUT VARCHAR2,
                      p_tArpfol OUT FACTU.TDU_TABLA_ARPFOL,
                      p_cNoOrder IN FACTU.ARPFOE.NO_ORDEN%TYPE) IS
                      
   CURSOR C_ARPFOL(cTag IN VARCHAR2) IS
       SELECT noCia,
        noOrden,
        grupo,
        noCliente,
        noArti,
        tipoArti,
        artiNuevo,
        bodega,
        cantComp,
        cantSolicitada, -- 10
        cantEntreg,
        cantAsignada,
        cantReasignada,
        fechaRegistro,
        precio,
        totLinea,
        estado,
        dsctoCliente,
        dPromo,
        igv, -- 20
        noLinea,
        pDscto3,
        mDscto2,
        mDscto3,
        impIgv,
        precioSigv,
        totalLin,
        descripcion,
        parte,
        tipoBs, -- 30
        indPideLote,
        operExoneradas,
        operGratuitas,
        operGravadas,
        operInafectas,
        tipoAfectacion,
        precIgv,
        medida -- 38
       FROM XMLTABLE(REPLACE('/*/arpfoe/arpfolList/arpfol','*',cTag) PASSING xmltype(p_cXmlInput)
        COLUMNS
             noCia VARCHAR2(2) PATH 'noCia',
             noOrden VARCHAR2(10) PATH 'noOrden',
             grupo VARCHAR2(2) PATH 'grupo',
             noCliente VARCHAR2(11) PATH 'noCliente',
             noArti VARCHAR2(25) PATH 'noArti',
             tipoArti VARCHAR2(1) PATH 'tipoArti',
             artiNuevo VARCHAR2(1) PATH 'artiNuevo',
             bodega VARCHAR2(5) PATH 'bodega',
             cantComp NUMBER(12,3) PATH 'cantComp',
             cantSolicitada NUMBER(12,3) PATH 'cantSolicitada', -- 10
             cantEntreg NUMBER(12,3) PATH 'cantEntreg',
             cantAsignada NUMBER(12,3) PATH 'cantAsignada',
             cantReasignada NUMBER(12,3) PATH 'cantReasignada',
             fechaRegistro DATE PATH 'fechaRegistro',
             precio NUMBER(23,13) PATH 'precio',
             totLinea NUMBER(13,5) PATH 'totLinea' ,
             estado VARCHAR2(1) PATH 'estado',
             dsctoCliente NUMBER(12,3) PATH 'dsctoCliente',
             dPromo NUMBER(5,3) PATH 'dPromo',
             igv NUMBER(5,3) PATH 'igv', -- 20
             noLinea NUMBER(3) PATH 'noLinea',
             pDscto3 NUMBER(6,3) PATH 'pDscto3',
             mDscto2 NUMBER(12,3) PATH 'mDscto2',
             mDscto3 NUMBER(12,3) PATH 'mDscto3',
             impIgv NUMBER(12,5) PATH 'impIgv',
             precioSigv NUMBER(23,13) PATH 'precioSigv',
             totalLin NUMBER(12,5) PATH 'totalLin',
             descripcion VARCHAR2(500) PATH 'descripcion',
             parte NUMBER(3) PATH 'parte',
             tipoBs VARCHAR2(1) PATH 'tipoBs', -- 30
             indPideLote VARCHAR2(1) PATH 'indPideLote',
             operExoneradas NUMBER(12,2) PATH 'operExoneradas',
             operGratuitas NUMBER(12,2) PATH 'operGratuitas',
             operGravadas NUMBER(12,2) PATH 'operGravadas',
             operInafectas NUMBER(12,2) PATH 'operInafectas',
             tipoAfectacion VARCHAR2(2) PATH 'tipoAfectacion',
             precIgv NUMBER(18,2) PATH 'precIgv',
             medida VARCHAR2(10) PATH 'medida'  -- 38
             );
     
    cNoOrder FACTU.ARPFOE.NO_ORDEN%TYPE;              
                      
  BEGIN
      
     p_tArpfol := FACTU.TDU_TABLA_ARPFOL();
     
     <<detalle>>
     FOR d IN C_ARPFOL(p_cTag) LOOP
        
       IF d.noCia IS NULL THEN
          p_cError := '1,En XML arpfol la etiqueta Código de compañia no puede ser nulo';
       END IF;
       
       IF d.noCliente IS NULL THEN
          p_cError := '1,En XML arpfol la etiqueta Código de cliente no puede ser nulo.';
       END IF;
       
       IF d.noArti IS NULL THEN
          p_cError := '1,En XML arpfol la etiqueta Código de articulo no puede ser nulo.';
       END IF;
       
       IF NVL(p_cNoOrder,'X') = 'X' THEN
          cNoOrder := d.noOrden;
       ELSE
          cNoOrder := p_cNoOrder;
       END IF;              
     
       IF p_cError != '0,OK' THEN
           DBMS_OUTPUT.PUT_LINE('>>> Error en el XML => '||p_cError);
           EXIT detalle;
       ELSE
          p_tArpfol.EXTEND;
          BEGIN
                      p_tArpfol(p_tArpfol.LAST) := OBJ_ARPFOL(
                                                d.noCia,
                                                cNoOrder,
                                                d.grupo,
                                                d.noCliente,
                                                d.noArti,
                                                d.tipoArti,
                                                d.artiNuevo,
                                                d.bodega,
                                                d.cantComp,
                                                d.cantSolicitada,
                                                d.cantEntreg,
                                                d.cantAsignada,
                                                d.cantReasignada,
                                                d.fechaRegistro,
                                                d.precio,
                                                d.totLinea,
                                                d.estado,
                                                d.dsctoCliente,
                                                d.dPromo,
                                                d.igv,
                                                d.noLinea,
                                                d.pDscto3,
                                                d.mDscto2,
                                                d.mDscto3,
                                                d.impIgv,
                                                d.precioSigv,
                                                d.totalLin,
                                                d.descripcion,
                                                d.parte,
                                                d.tipoBs,
                                                d.indPideLote,
                                                d.operExoneradas,
                                                d.operGratuitas,
                                                d.operGravadas,
                                                d.operInafectas,
                                                d.tipoAfectacion,
                                                d.precIgv,
                                                d.medida
                                                );
          EXCEPTION
                 WHEN VALUE_ERROR THEN
                    p_cError := '1,Error2 VALUE_ERROR: ' || SQLERRM;
                    DBMS_OUTPUT.PUT_LINE('Error2 VALUE_ERROR: ' || SQLERRM);
                 WHEN OTHERS THEN
                    p_cError := '1,Error2 desconocido: ' || SQLERRM;
                    DBMS_OUTPUT.PUT_LINE('Error2 desconocido: ' || SQLERRM);
          END;
       END IF;
       
     END LOOP detalle;
          
  EXCEPTION
    WHEN VALUE_ERROR THEN
        p_cError := '1,Error SET_TDU_ARPFOL -> VALUE_ERROR: ' || SQLERRM;
        DBMS_OUTPUT.PUT_LINE('Error VALUE_ERROR: ' || SQLERRM);
    WHEN OTHERS THEN       
       p_cError := '1, FACTU.PR_COMPROBANTE_PAGO.SET_TDU_ARPFOL ==> '||SQLERRM;  
  
  END SET_TDU_ARPFOL;

END PR_PEDIDO;
/