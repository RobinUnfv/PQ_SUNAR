CREATE OR REPLACE PACKAGE BODY FACTU.PR_GUIA IS

  /*---------------------------------------------------------------------------------------
   Nombre      : GET_GUIA_FICTA
   Proposito   : PROCEDIMIENTO QUE NOS VA PERMITIR GENERAR UN GUIA FICTA
   Parametro  :             

   Log de Cambios:
     Fecha        Autor                     Descripción
     11/10/2024   Robinzon Santana          Creador   
  -----------------------------------------------------------------------------------------*/
  FUNCTION GET_GUIA_FICTA(p_cNocia IN FACTU.ARFACF.NO_CIA%TYPE,
                           p_cCentro IN FACTU.ARFACF.CENTRO%TYPE,
                           p_cError IN OUT VARCHAR2
                           ) RETURN FACTU.ARFAFE.NO_GUIA%TYPE IS
                           
   cNoGuia FACTU.ARFAFE.NO_GUIA%TYPE;
      
  BEGIN

    SELECT SERIE_GR||LPAD(TO_CHAR(NVL( NVL(CORREL_FICT,0)+1,0)),7,'0')
    INTO cNoGuia
    FROM FACTU.ARFACF
    WHERE NO_CIA = p_cNocia
    AND CENTRO = p_cCentro;
    
    RETURN cNoGuia;
  
  EXCEPTION
    WHEN OTHERS THEN
       p_cError := '1,ERROR EN FACTU.PR_GUIA.GET_GUIA_FICTA';
       cNoGuia := '0';
       RETURN cNoGuia;
  
  END GET_GUIA_FICTA;
  
 /*---------------------------------------------------------------------------------------
   Nombre      : UPDATE_GUIA_FICTA
   Proposito   : PROCEDIMIENTO QUE ACTUALIZA LA GUIA FICTA
   Parametro  :
             
   Log de Cambios:
     Fecha        Autor                     Descripción
     11/10/2024   Robinzon Santana          Creador   
 -----------------------------------------------------------------------------------------*/
 PROCEDURE UPDATE_GUIA_FICTA(p_cNocia IN FACTU.ARFACF.NO_CIA%TYPE,
                           p_cCentro IN FACTU.ARFACF.CENTRO%TYPE,
                           p_cError IN OUT VARCHAR2) IS
                           
   nCorrelFict FACTU.ARFACF.CORREL_FICT%TYPE;
   
 BEGIN
   
   SELECT NVL(CORREL_FICT,0)+1
   INTO nCorrelFict
   FROM FACTU.ARFACF
   WHERE NO_CIA = p_cNocia
   AND CENTRO = p_cCentro;
   
   UPDATE FACTU.ARFACF
   SET CORREL_FICT = nCorrelFict
   WHERE NO_CIA = p_cNocia
   AND CENTRO = p_cCentro;
   
   IF SQL%ROWCOUNT = 0 THEN
      p_cError := '1,NO SE ACTUALIZO EL CORRELATIVO DE LA GUIA FICTA.';
   END IF;
 
 EXCEPTION
    WHEN OTHERS THEN
       p_cError := '1,ERROR EN FACTU.PR_GUIA.UPDATE_GUIA_FICTA';
 
 END UPDATE_GUIA_FICTA;
 
   /*---------------------------------------------------------------------------------------
   Nombre      : GET_GUIA_FICTA
   Proposito   : FUNCION QUE NOS DEVUELTE EL NO_DOCU DE LA GUIA
   Parametro  :             

   Log de Cambios:
     Fecha        Autor                     Descripción
     12/10/2024   Robinzon Santana          Creador   
  -----------------------------------------------------------------------------------------*/
  FUNCTION GET_NO_DOCU( p_cNocia IN INVE.ARINSE.NO_CIA%TYPE,
                        p_cBodega IN INVE.ARINSE.BODEGA%TYPE,
                        p_cTipoDoc IN INVE.ARINSE.TIPO_DOC%TYPE,
                        p_cError IN OUT VARCHAR2
                         ) RETURN INVE.ARINSE.SECUENCIA%TYPE IS
                         
    nSecuencia INVE.ARINSE.SECUENCIA%TYPE := 0;
    
  BEGIN
  
    SELECT NVL(secuencia,0)+1
    INTO nSecuencia
    FROM INVE.ARINSE
    WHERE NO_CIA = p_cNocia
    AND BODEGA = p_cBodega
    AND TIPO_DOC = p_cTipoDoc;
    
    RETURN nSecuencia;
    
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
       p_cError := '1,LA CONSULTA NO TRAE DATOS EN PR_GUIA.GET_NO_DOCU. NO_CIA = '||
                 p_cNocia||' , BODEGA =  '||p_cBodega||' , TIPO_DOC = '||p_cTipoDoc;
       RETURN nSecuencia;
    WHEN TOO_MANY_ROWS THEN
       p_cError := '1,LA CONSULTA TRAE MÁS DE DOS DATOS EN PR_GUIA.GET_NO_DOCU. NO_CIA = '||
                 p_cNocia||' , BODEGA =  '||p_cBodega||' , TIPO_DOC = '||p_cTipoDoc;
       RETURN nSecuencia;
    WHEN OTHERS THEN
       p_cError := '1,ERROR EN PR_GUIA.GET_NO_DOCU. NO_CIA = '||
                 p_cNocia||' , BODEGA =  '||p_cBodega||' , TIPO_DOC = '||p_cTipoDoc;
       RETURN nSecuencia;
       
  END GET_NO_DOCU;
  
     /*---------------------------------------------------------------------------------------
   Nombre      : UPDATE_NO_DOCU
   Proposito   : PROCEDIMIENTO QUE NOS VA PERMITIR ACTUALIZAR LA SECUENCIA DEL NO_DOCU
   Parametro  :             

   Log de Cambios:
     Fecha        Autor                     Descripción
     12/10/2024   Robinzon Santana          Creador   
  -----------------------------------------------------------------------------------------*/
  PROCEDURE UPDATE_NO_DOCU( p_cNocia IN INVE.ARINSE.NO_CIA%TYPE,
                        p_cBodega IN INVE.ARINSE.BODEGA%TYPE,
                        p_cTipoDoc IN INVE.ARINSE.TIPO_DOC%TYPE,
                        p_nSecuencia IN INVE.ARINSE.SECUENCIA%TYPE,
                        p_cError IN OUT VARCHAR2
                         ) IS
  
  BEGIN
    
     UPDATE INVE.ARINSE
     SET SECUENCIA = p_nSecuencia
     WHERE NO_CIA = p_cNocia
     AND BODEGA = p_cBodega
     AND TIPO_DOC = p_cTipoDoc;
     
     IF SQL%ROWCOUNT = 0 THEN
        p_cError := '1,NO SE PUDO ACTULIZAR EL NODUCU';
     END IF;
  
  END UPDATE_NO_DOCU;
  
  /*---------------------------------------------------------------------------------------
   Nombre      : GUARDAR_GUIA_FICTA
   Proposito   : PROCEDIMIENTO QUE NOS PERMITE GUARDAR LA GUIA FICTA
   Parametro  :             

   Log de Cambios:
     Fecha        Autor                     Descripción
     13/10/2024   Robinzon Santana          Creador
     03/04/2025   Robinzon Santana          Modificando la fecha por la fecha actual 
  -----------------------------------------------------------------------------------------*/
  PROCEDURE GUARDAR_GUIA_FICTA(p_cNoCia IN FACTU.ARPFOE.NO_CIA%TYPE,
                               p_cNoOrden IN FACTU.ARPFOE.NO_ORDEN%TYPE,
                               p_cNoGuia OUT FACTU.ARFAFE.NO_GUIA%TYPE,
                               p_nNoDocu OUT INVE.ARINSE.SECUENCIA%TYPE,                      
                               p_cError IN OUT VARCHAR2
                              ) IS
                              
    CURSOR C_ARPFOE IS
        SELECT BODEGA, FECHA_ENTREGA, GRUPO, NO_CLIENTE,NO_VENDEDOR,CENTRO,COD_T_PED,
               ALMA_DESTINO, COD_DIR_SALIDA, RUC
        FROM FACTU.ARPFOE
        WHERE NO_CIA = p_cNoCia
        AND NO_ORDEN = p_cNoOrden;
    
    CURSOR C_ARPFOL IS
        SELECT NO_ARTI, DESCRIPCION, CANT_SOLICITADA, 
        NO_LINEA, TIPO_BS
        FROM FACTU.ARPFOL
        WHERE NO_CIA = p_cNoCia
        AND NO_ORDEN = p_cNoOrden;
        
    cBodega FACTU.ARPFFE.BODEGA%TYPE;
    dFechaEntrega FACTU.ARPFFE.FECHA_ENTREGA%TYPE;
    cGrupo FACTU.ARPFFE.GRUPO%TYPE;
    cNoCliente FACTU.ARPFFE.NO_CLIENTE%TYPE;
    cNoVendedor FACTU.ARPFFE.NO_VENDEDOR%TYPE;
    cCentro FACTU.ARPFFE.CENTRO%TYPE;
    cCodTPed FACTU.ARPFOE.COD_T_PED%TYPE;
    cAlmaDestino FACTU.ARPFOE.ALMA_DESTINO%TYPE;
    cCodDirSalida FACTU.ARPFOE.COD_DIR_SALIDA%TYPE;
    cRuc FACTU.ARPFOE.RUC%TYPE;
        
    cNoGuia FACTU.ARPFFE.NO_GUIA%TYPE;
    nNoDocu FACTU.ARPFFE.NO_DOCU%TYPE;
    cDescrip FACTU.ARPFFE.DESCRIPCION%TYPE;
    
    kEstado CONSTANT CHAR(1) := 'D';
    kTipo CONSTANT CHAR(1) := 'V';
    kClase CONSTANT CHAR(1) := 'V';
    
    cPuntoPartida INVE.ARINBO1.DIRECCION%TYPE;
    cNombTienda CXC.ARCCTDA.NOMBRE%TYPE;
    cPuntoLLegada CXC.ARCCTDA.DIRECCION%TYPE;
    cDescDist CXC.ARCCDI.DESC_DIST%TYPE;
    cDescProv CXC.ARCCPR.DESC_PROV%TYPE;
    cDescDepa CXC.ARCCDP.DESC_DEPA%TYPE;
    cRazSocDest CXC.ARCCMC.NOMBRE%TYPE;
    
  BEGIN
     
    DBMS_OUTPUT.PUT_LINE('>>> ENTRO A GUIA FICTA ');   
  
     FOR i in C_ARPFOE LOOP
        
        cBodega := i.BODEGA;
        -- dFechaEntrega := i.FECHA_ENTREGA; --  03-04-2025 /  Robinzon Santana/ Modificando la fecha por la fecha actual 
        cGrupo := i.GRUPO;
        cNoCliente := i.NO_CLIENTE;
        cNoVendedor := i.NO_VENDEDOR;
        cCentro := i.CENTRO;
        cCodTPed := i.COD_T_PED;
        cAlmaDestino := i.ALMA_DESTINO;
        cCodDirSalida := i.COD_DIR_SALIDA;
        cRuc := i.RUC;
     END LOOP;
     
     cNoGuia := FACTU.PR_GUIA.GET_GUIA_FICTA(p_cNoCia, cCentro, p_cError);
     
     IF p_cError = '0,OK' THEN
             
        nNoDocu := FACTU.PR_GUIA.GET_NO_DOCU(p_cNoCia, cBodega, cCodTPed, p_cError);
        
        IF p_cError = '0,OK' THEN
           
           cDescrip := 'GENERADO POR INVENTARIOS - TRANSC : '||cCodTPed||' NO DOC. : '||nNoDocu;
           
           BEGIN
                SELECT direccion
                INTO cPuntoPartida
                FROM INVE.ARINBO1 bo
                WHERE no_cia = p_cNoCia
                AND codigo = cBodega
                AND estado = 'A';
           EXCEPTION
             WHEN NO_DATA_FOUND THEN
                p_cError := '1,NO TIENE PUNTO DE PARTIDA';
             WHEN TOO_MANY_ROWS THEN
                p_cError := '1,TIENE DOS PUNTOS DE PARTIDAS';
             WHEN OTHERS THEN
               p_cError := '1,ERROR PARA OBTENER EL PUNTO DE PARTIDA';
           END;
           
           IF p_cError = '0,OK' THEN
                 
               BEGIN
                    SELECT Z.nombre, Z.direccion, Q.desc_dist , Y.desc_prov , X.desc_depa
                    INTO cNombTienda, cPuntoLLegada, cDescDist, cDescProv, cDescDepa
                    FROM CXC.ARCCTDA Z, CXC.ARCCDP X, CXC.ARCCPR Y, CXC.ARCCDI Q
                    WHERE Z.NO_CIA = p_cNoCia
                    AND Z.NO_CLIENTE = cNoCliente
                    AND Z.COD_TIENDA = '001'
                    AND X.NO_CIA= Z.NO_CIA
                    AND X.CODI_DEPA = Z.CODI_DEPA
                    AND Y.NO_CIA = Z.NO_CIA
                    AND Y.CODI_DEPA = Z.CODI_DEPA
                    AND Y.CODI_PROV = Z.CODI_PROV
                    AND Q.NO_CIA = Z.NO_CIA
                    AND Q.CODI_DEPA = Z.CODI_DEPA
                    AND Q.CODI_PROV = Z.CODI_PROV
                    AND Q.CODI_DIST = Z.CODI_DIST;
               EXCEPTION
                 WHEN NO_DATA_FOUND THEN
                    p_cError := '1,NO TIENE PUNTO DE LLEGADA';
                 WHEN TOO_MANY_ROWS THEN
                    p_cError := '1,TIENE DOS PUNTO DE LLEGADA';
                 WHEN OTHERS THEN
                    p_cError := '1,ERROR PARA OBTENER EL PUNTO DE LLEGADA';
               END;
               
               IF p_cError = '0,OK' THEN
                  
                   BEGIN
                        SELECT NOMBRE
                        INTO cRazSocDest
                        FROM CXC.ARCCMC
                        WHERE NO_CIA = p_cNoCia
                        AND NO_CLIENTE = cNoCliente;
                   EXCEPTION
                     WHEN NO_DATA_FOUND THEN
                        p_cError := '1,NO TIENE RAZON SOCIAL DESTINATARIO';
                     WHEN TOO_MANY_ROWS THEN
                        p_cError := '1,TIENE DOS RAZON SOCIAL DESTINATARIO';
                     WHEN OTHERS THEN
                       p_cError := '1,ERROR PARA OBTENER RAZON SOCIAL DESTINATARIO';
                   END;
                   
                   IF p_cError = '0,OK' THEN
                      
                      BEGIN
                          INSERT INTO FACTU.ARPFFE (NO_CIA, BODEGA, NO_GUIA, FECHA, GRUPO, NO_CLIENTE,
                                                    NO_VENDEDOR, DESCRIPCION, NO_ORDEN, ESTADO,
                                                    CENTRO, TIPO, CLASE, NO_DOCU, TIPO_TRANSC, FECHA_INICIO,
                                                    PUNTO_PARTIDA, PUNTO_LLEGADA, LLEGADA_DISTRITO, LLEGADA_PROVINCIA, LLEGADA_DEPARTAMENTO, RAZON_SOCIAL_DESTINATARIO,
                                                    RUC_DESTINATARIO, MOTIVO_TRASLADO, TIPO_DOC_REF, COD_FPAGO, ALMA_ORIGEN, ALMA_DESTINO,
                                                    NOMB_TIENDA, COD_TIENDA, COD_DIR_ENTREGA, COD_DIR_SALIDA,
                                                    TIPO_GUIA, IMPRIME, IND_FICTA, IND_PROFORMA)
                          /*  03-04-2025 /  Robinzon Santana/ Modificando la fecha por la fecha actual
                          VALUES(p_cNoCia, cBodega, cNoGuia, dFechaEntrega, cGrupo, cNoCliente,
                             cNoVendedor, cDescrip, p_cNoOrden, kEstado,
                             cCentro, kTipo, kClase, nNoDocu, cCodTPed, dFechaEntrega,
                             cPuntoPartida, cPuntoLLegada, cDescDist, cDescProv, cDescDepa, cRazSocDest,
                             cRuc, '1', 'OC', '01', cBodega, cAlmaDestino,
                             cNombTienda, '001', '001', '201',
                             'GR', 'S', 'S','N');
                             */
                             VALUES(p_cNoCia, cBodega, cNoGuia, sysdate, cGrupo, cNoCliente,
                             cNoVendedor, cDescrip, p_cNoOrden, kEstado,
                             cCentro, kTipo, kClase, nNoDocu, cCodTPed, sysdate,
                             cPuntoPartida, cPuntoLLegada, cDescDist, cDescProv, cDescDepa, cRazSocDest,
                             cRuc, '1', 'OC', '01', cBodega, cAlmaDestino,
                             cNombTienda, '001', '001', '201',
                             'GR', 'S', 'S','N');
                      EXCEPTION
                        WHEN OTHERS THEN
                          p_cError := '1,NO SE PUDO REGISTRAR LA CABECERA DE LA GUIA. '||SQLERRM;
                      END;
                   
                   END IF;
               
               END IF;
               
           END IF;

        END IF;
     
     END IF;
     
     -- DETALLE DE LA GUIA FICTA
     IF p_cError = '0,OK' THEN
        
        FOR i in C_ARPFOL LOOP
            
            BEGIN
               INSERT INTO FACTU.ARPFFL(NO_CIA, BODEGA, NO_GUIA, NO_ARTI, DESCRIPCION, 
                         CANTIDAD, IND_PARENTESCO, NO_LINEA, TIPO_BS )
               VALUES(p_cNoCia, cBodega, cNoGuia, i.NO_ARTI, i.DESCRIPCION,
                     i.CANT_SOLICITADA, 'N', i.NO_LINEA, i.TIPO_BS);
            EXCEPTION
                WHEN OTHERS THEN
                   p_cError := '1,NO SE PUDO REGISTRAR EL DETALLE DE LA GUIA. '||SQLERRM;
            END;           
                
        END LOOP;
     
     END IF;
     
     IF p_cError = '0,OK' THEN
        FACTU.PR_GUIA.UPDATE_NO_DOCU(p_cNoCia, cBodega, cCodTPed, nNoDocu, p_cError);
     END IF;
     
             
     IF p_cError = '0,OK' THEN
           FACTU.PR_GUIA.UPDATE_GUIA_FICTA(p_cNoCia, cCentro, p_cError);
     END IF;
        
     p_cNoGuia := cNoGuia;
     p_nNoDocu := nNoDocu;
  
  END GUARDAR_GUIA_FICTA;
  
  /*---------------------------------------------------------------------------------------
   Nombre      : FACTURAR_GUIA_FICTA
   Proposito   : FACTURAR GUIA FICTA
   Parametro  :             

   Log de Cambios:
     Fecha        Autor                     Descripción
     14/10/2024   Robinzon Santana          Creador   
  -----------------------------------------------------------------------------------------*/
  PROCEDURE FACTURAR_GUIA_FICTA(p_cNoCia IN FACTU.ARPFFE.NO_CIA%TYPE,
                                p_cBodega IN FACTU.ARPFFE.BODEGA%TYPE,
                                p_cNoGuia IN FACTU.ARPFFE.NO_GUIA%TYPE,
                                p_cTipoDoc IN FACTU.ARPFFE.TIPO_DOC%TYPE,
                                p_cNoFactu IN FACTU.ARPFFE.NO_FACTU%TYPE,
                                p_cError IN OUT VARCHAR2
                               ) IS
                               
     kEstado CONSTANT CHAR(1) := 'F';
     cIndFactura FACTU.ARPFFE.IND_FACTURA%TYPE;
     cIndBoleta FACTU.ARPFFE.IND_BOLETA%TYPE;
                               
  BEGIN
     
    IF p_cTipoDoc = 'F' THEN
       cIndFactura := 'S';
       cIndBoleta := 'N';
    END IF;
    
    IF p_cTipoDoc = 'B' THEN
       cIndBoleta := 'S';
       cIndFactura := 'N';
    END IF;
    
    UPDATE FACTU.ARPFFE
    SET ESTADO = kEstado,
        TIPO_DOC = p_cTipoDoc, 
        NO_FACTU = p_cNoFactu,
        IND_FACTURA = cIndFactura,
        IND_BOLETA = cIndBoleta
    WHERE NO_CIA = p_cNoCia
    AND BODEGA = p_cBodega
    AND NO_GUIA = p_cNoGuia;
    
    IF SQL%ROWCOUNT = 0 THEN
       p_cError := '1,NO SE PUDO FACTURAR LA GUIA. '||SQLERRM;
    END IF;
  
  END FACTURAR_GUIA_FICTA;
  
END PR_GUIA;
/