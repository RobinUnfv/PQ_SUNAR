CREATE OR REPLACE PACKAGE BODY FACTU.PR_DOCUMENTO IS
  
  /*-----------------------------------------------------------------------------
  Nombre      : OBTENER_CORRELATIVO
  Proposito   : OBTENER EL NUMERO CORRELATIVO ACTUAL
  Referencias :
  Parametros  :P_cNoCia         VARCHAR2    Numero de compañia
               P_cCentro        VARCHAR2    Centro de costo
               P_cTipoDoc       VARCHAR2    Tipo de documento
               P_sSerie         VARCHAR2    Serie del documento                           

  Log de Cambios
    Fecha         Autor                      Descripcion
    24/05/2024   ROBINZON SANTANA           Creacion
    11/09/2024   Robinzon Santana           Se agrego la columna ACTIVO en el valor por defecto 'S'
  ----------------------------------------------------------------------------*/
  FUNCTION OBTENER_CORRELATIVO(P_cNoCia IN FACTU.ARFACC.NO_CIA%TYPE,
                            P_cCentro  IN FACTU.ARFACC.CENTRO%TYPE,
                            P_cTipoDoc IN FACTU.ARFACC.TIPO_DOC%TYPE,
                            P_cSerie   IN FACTU.ARFACC.SERIE%TYPE) RETURN NUMBER 
  IS
   
    nConsDesde FACTU.ARFACC.CONS_DESDE%TYPE;
    
  BEGIN
     dbms_output.put_line('INICIO --> PR_DOCUMENTO.OBTENER_CORRELATIVO ');
     
     SELECT CONS_DESDE
     INTO nConsDesde
     FROM FACTU.ARFACC
     WHERE NO_CIA = P_cNoCia
     AND CENTRO   = P_cCentro
     AND TIPO_DOC = P_cTipoDoc
     AND SERIE    = P_cSerie
     AND ACTIVO   = 'S';
     
     RETURN nConsDesde;
     
  EXCEPTION
     WHEN OTHERS THEN
        RAISE_APPLICATION_ERROR(-2000,'Hubo un problema para obtener el correlativo del documento : '||NVL(P_cTipoDoc,'-')||', no_cia : '||NVL(P_cNoCia,'-')
         ||' , SERIE : '||NVL(P_cSerie,'-'));
        RETURN -1;
        
  END OBTENER_CORRELATIVO;
  
    /*-----------------------------------------------------------------------------
  Nombre      : ACTUALIZAR_CORRELATIVO
  Proposito   : PROCEDIMIENTO PARA ACTUALIZAR EL CORRELATIVO
  Referencias :
  Parametros  :P_cNoCia         VARCHAR2    Numero de compañia
               P_cCentro        VARCHAR2    Centro de costo
               P_cTipoDoc       VARCHAR2    Tipo de documento
               P_sSerie         VARCHAR2    Serie del documento
               P_nConsDesde     NUMBER     Correlativo del documento                          

  Log de Cambios
    Fecha         Autor                      Descripcion
    07/09/2024   ROBINZON SANTANA           Creacion
    11/09/2024   Robinzon Santana           Se agrego la columna ACTIVO en el valor por defecto 'S'
  ----------------------------------------------------------------------------*/
  PROCEDURE ACTUALIZAR_CORRELATIVO (P_cNoCia IN FACTU.ARFACC.NO_CIA%TYPE,
                            P_cCentro  IN FACTU.ARFACC.CENTRO%TYPE,
                            P_cTipoDoc IN FACTU.ARFACC.TIPO_DOC%TYPE,
                            P_cSerie   IN FACTU.ARFACC.SERIE%TYPE,
                            P_nConsDesde OUT FACTU.ARFACC.CONS_DESDE%TYPE,
                            p_cError IN OUT VARCHAR2,
                            p_cXMLError IN OUT NOCOPY XMLTYPE) IS
    
    nConsDesde   FACTU.ARFACC.CONS_DESDE%TYPE;
    
  BEGIN
       DBMS_OUTPUT.Put_Line(' -----------------------------');
       DBMS_OUTPUT.Put_Line(' PR_DOCUMENTO.ACTUALIZAR_CORRELATIVO => P_cTipoDoc '||P_cTipoDoc||' , P_cSerie  '||P_cSerie);
       nConsDesde := OBTENER_CORRELATIVO(P_cNoCia,P_cCentro,P_cTipoDoc,P_cSerie);
        
        IF nConsDesde < 0 THEN
           P_cError := '1,El documento : '||P_cTipoDoc||' no tiene correlativo.';
           FACTU.PR_PEDIDO.PUSH_ERROR('ACTUALIZAR_CORRELATIVO', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError);
        END IF;
        
        IF P_cError = '0,OK' THEN
            
            nConsDesde := nConsDesde + 1;
            DBMS_OUTPUT.Put_Line('PR_DOCUMENTO.ACTUALIZAR_CORRELATIVO => CONS_DESDE '||nConsDesde||' , P_cNoCia '||P_cNoCia||' , P_cCentro '
                  ||P_cCentro||' , P_cTipoDoc '||P_cTipoDoc||' , P_cSerie '||P_cSerie);
        
            UPDATE FACTU.ARFACC
            SET CONS_DESDE = nConsDesde
            WHERE NO_CIA = P_cNoCia
            AND CENTRO   = P_cCentro
            AND TIPO_DOC = P_cTipoDoc
            AND SERIE    = P_cSerie
            AND ACTIVO   = 'S';
            
            IF SQL%ROWCOUNT > 0 THEN
               P_nConsDesde := nConsDesde;
            END IF;
            
        END IF;
        
  EXCEPTION
    WHEN OTHERS THEN
       p_cError := '1,ERROR AL ACTUALIZAR EN FACTU.ARFACC : '||SQLERRM;
       FACTU.PR_PEDIDO.PUSH_ERROR('ACTUALIZAR_CORRELATIVO', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError);
  END ACTUALIZAR_CORRELATIVO;
  
  
  /*-----------------------------------------------------------------------------
  Nombre      : GET_SERIE_FE_CAJA
  Proposito   : CAPTURAR SERIE DE UN COMPROBANTE ELECTRONICO POR CAJA
  Referencias :
  Parametros  :P_cNoCia         VARCHAR2    Numero de compañia
               P_cCentro        VARCHAR2    Centro de costo
               P_cTipoDoc       VARCHAR2    Tipo de documento
               p_noCaba         VARCHAR2    Codigo de la cajera
               P_sSerie         VARCHAR2    Serie del documento                           

  Log de Cambios
    Fecha         Autor                      Descripcion
    11/09/2024   ROBINZON SANTANA           Creacion
  ----------------------------------------------------------------------------*/
  PROCEDURE GET_SERIE_FE_CAJA(p_cNoCia IN FACTU.ARFACC.NO_CIA%TYPE,
                            p_cCentro IN FACTU.ARFACC.CENTRO%TYPE,
                            p_cTipoDoc IN FACTU.ARFACC.TIPO_DOC%TYPE,                            
                            p_cNoCaba IN FACTU.ARFACC.NO_CABA%TYPE,
                            p_cSerie OUT FACTU.ARFACC.SERIE%TYPE,
                            p_cError IN OUT VARCHAR2,
                            p_cXMLError IN OUT NOCOPY XMLTYPE) IS
                            
     cSerie FACTU.ARFACC.SERIE%TYPE;
  
  BEGIN
     
     BEGIN
         SELECT SERIE
         INTO p_cSerie
         FROM FACTU.ARFACC
         WHERE NO_CIA = p_cNoCia
         AND CENTRO = p_cCentro
         AND TIPO_DOC = p_cTipoDoc
         AND SUBSTR(SERIE,1,1) = p_cTipoDoc
         AND NO_CABA = p_cNoCaba
         AND ACTIVO = 'S';
     EXCEPTION
       WHEN TOO_MANY_ROWS THEN
       
             SELECT SERIE
             INTO p_cSerie
             FROM FACTU.ARFACC
             WHERE NO_CIA = p_cNoCia
             AND CENTRO = p_cCentro
             AND TIPO_DOC = p_cTipoDoc
             AND SUBSTR(SERIE,1,1) = p_cTipoDoc
             AND NO_CABA = p_cNoCaba
             AND ACTIVO = 'S'
             AND ROWNUM = 1;
       
       WHEN OTHERS THEN
         p_cSerie := NULL;
         p_cError := '1,No tiene registrado SERIE para cia : '||p_cNoCia||' , centro : '
                      ||p_cCentro||' , tipo de documento : '||p_cTipoDoc||' . caja : '||p_cNoCaba;
         FACTU.PR_PEDIDO.PUSH_ERROR('PR_DOCUMENTO.GET_SERIE_FE_CAJA', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError);
       
     END;  
  
  END GET_SERIE_FE_CAJA;
  
    
  /*-----------------------------------------------------------------------------
  Nombre      : GET_NUM_DOCUMENTO
  Proposito   :PROCEDIMIENTO QUE NOS PERMITA CAPTURAR EL NUMERO DEL DOCUMENTO
  Referencias :
  Parametros  :P_cNoCia         VARCHAR2    Numero de compañia
               P_cCentro        VARCHAR2    Centro de costo
               P_cTipoDoc       VARCHAR2    Tipo de documento
               P_sSerie         VARCHAR2    Serie del documento
               P_nConsDesde     NUMBER     Correlativo del documento                          

  Log de Cambios
    Fecha         Autor                      Descripcion
    08/10/2024   ROBINZON SANTANA           Creacion
    03/03/2026   ROBINZON SANTANA           <RR4-01> Modificación el tipo para aceptar nota de venta
  ----------------------------------------------------------------------------*/
  FUNCTION GET_NUM_DOCUMENTO(p_cNoCia IN FACTU.ARFACC.NO_CIA%TYPE,
                              p_cCentro  IN FACTU.ARFACC.CENTRO%TYPE,
                              p_cTipoDoc IN FACTU.ARFACC.TIPO_DOC%TYPE,
                              p_cError IN OUT VARCHAR2
                             ) RETURN VARCHAR2 IS
                             
    kEstadoSerie  CONSTANT CHAR(1) := 'S';
    cNoOrden      FACTU.ARPFOE.NO_ORDEN%TYPE;
    cDescCompPago VARCHAR2(15);
    cNoFactu      FACTU.ARFAFE.NO_FACTU%TYPE;
    
  BEGIN
    
   -- <RR4-01> IF p_cTipoDoc = 'P' THEN
   IF p_cTipoDoc IN('P','NV') THEN
        BEGIN
            SELECT SERIE||LPAD(TO_CHAR(NVL(TO_NUMBER(cons_desde) + 1,0)),7,'0')
            INTO cNoOrden
            FROM FACTU.ARFACC
            WHERE NO_CIA = p_cNoCia
            AND CENTRO   = p_cCentro
            AND TIPO_DOC = p_cTipoDoc
            AND ACTIVO   = kEstadoSerie;
        EXCEPTION
          WHEN NO_DATA_FOUND THEN
             p_cError := '1,NO TIENE CONFIGURADO SERIE EL PEDIDO PARA LOS PARAMETROS: CIA = '||p_cNoCia||' , CENTRO = '||
                          p_cCentro||' , TIPO DOC. = '||p_cTipoDoc||' , ACTIVO = '||kEstadoSerie;
          WHEN TOO_MANY_ROWS THEN
             p_cError := '1,TIENE MÁS DE UNA CONFIGURACIÓN EL PEDIDO PARA LOS PARAMETROS: CIA = '||p_cNoCia||' , CENTRO = '||
                          p_cCentro||' , TIPO DOC. = '||p_cTipoDoc||' , ACTIVO = '||kEstadoSerie;
          WHEN OTHERS THEN
              p_cError := '1,HUBO UN ERROR PARA OBTENER LA SERIE, CIA = '||p_cNoCia||' , CENTRO = '||
                          p_cCentro||' , TIPO DOC. = '||p_cTipoDoc||' , ACTIVO = '||kEstadoSerie;
        END;
        DBMS_OUTPUT.Put_Line(' PEDIDO = '||cNoOrden);
        RETURN cNoOrden;
        
    ELSIF p_cTipoDoc IN('B','F') THEN
        
        IF p_cTipoDoc = 'B' THEN
           cDescCompPago := 'BOLETA';
        ELSE
           cDescCompPago := 'FACTURA';
        END IF;
        
        BEGIN
            SELECT SERIE||LPAD(TO_CHAR(NVL(TO_NUMBER(cons_desde) + 1,0)),7,'0')
            INTO cNoFactu
            FROM FACTU.ARFACC
            WHERE NO_CIA = p_cNoCia
            AND CENTRO   = p_cCentro
            AND TIPO_DOC = p_cTipoDoc
            AND SUBSTR(SERIE,1,1) = p_cTipoDoc
            AND ACTIVO   = kEstadoSerie;
        EXCEPTION
          WHEN NO_DATA_FOUND THEN
             p_cError := '1,'|| cDescCompPago ||' NO TIENE CONFIGURADO SERIE. LA PARA LOS PARAMETROS: CIA = '||p_cNoCia||' , CENTRO = '||
                          p_cCentro||' , TIPO DOC. = '||p_cTipoDoc||' , ACTIVO = '||kEstadoSerie;
          WHEN TOO_MANY_ROWS THEN
             p_cError := '1,'|| cDescCompPago ||' TIENE MÁS DE UNA SERIE CONFIGURADA. LA '|| cDescCompPago ||' PARA LOS PARAMETROS: CIA = '||p_cNoCia||' , CENTRO = '||
                          p_cCentro||' , TIPO DOC. = '||p_cTipoDoc||' , ACTIVO = '||kEstadoSerie;
          WHEN OTHERS THEN
             p_cError := '1, ERROR EN PR_DOCUMENTO.GET_NUM_DOCUMENTO. LA '|| cDescCompPago ||' PARA LOS PARAMETROS: CIA = '||p_cNoCia||' , CENTRO = '||
                          p_cCentro||' , TIPO DOC. = '||p_cTipoDoc||' , ACTIVO = '||kEstadoSerie;
        END;
        DBMS_OUTPUT.Put_Line(cDescCompPago||' => '||cNoFactu);
        RETURN cNoFactu;
        
    END IF;
  
  END GET_NUM_DOCUMENTO;
  

END PR_DOCUMENTO;
/