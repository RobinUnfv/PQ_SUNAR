CREATE OR REPLACE PACKAGE BODY FACTU.PR_FACTURA IS


 PROCEDURE DUPLICADO_FACTURA(
        p_cNoCia     IN FACTU.ARFAFE.NO_CIA%TYPE,
        p_cTipoDoc   IN FACTU.ARFACC.TIPO_DOC%TYPE DEFAULT 'F',
        p_cNoFactu   IN FACTU.ARFAFE.NO_FACTU%TYPE,
        p_cNoOrden   IN FACTU.ARFAFE.NO_ORDEN%TYPE,
        p_dFecha     IN FACTU.ARFAFE.FECHA%TYPE,
        p_cNoCliente IN FACTU.ARFAFE.NO_CLIENTE%TYPE,
        p_nNumCopia  IN NUMBER DEFAULT 1,
        --
        p_cCentro    IN FACTU.ARFACC.CENTRO%TYPE,        
        p_cSerie     IN FACTU.ARFACC.SERIE%TYPE,
        --
        p_cError     IN OUT VARCHAR2
        ) IS
        
 i NUMBER(4);
 
 CURSOR C_ARFAFE IS
    SELECT NO_CIA,TIPO_DOC,NO_FACTU,GRUPO,NO_CLIENTE,CENTRO,BODEGA,FECHA,TIPO_CLIENTE,
       NBR_CLIENTE,NO_VENDEDOR,TIPO_PRECIO,MONEDA,TIPO_CAMBIO,NO_ORDEN,SUB_TOTAL,IMPUESTO,
       TOTAL,ESTADO,COD_FPAGO,NO_GUIA,TIPO,DIVISION,FECHA_ENTREGA,MOTIVO_TRASLADO,RUC,IND_DOC,
       COD_T_PED,COD_CLAS_PED,TIPO_FPAGO,VALOR_VENTA,M_DSCTO_GLOBAL,FECHA_VENCE,CODI_DEPA,CODI_PROV,
       CODI_DIST,NO_DOCU,COD_TIENDA,COD_DIR_ENTREGA,TIPO_DOC_CLI,NUM_DOC_CLI,IMPRIME,IND_VTA_ANTICIPADA,
       TOTAL_BRUTO,T_DESCUENTO,IND_PVENT,COD_CAJA,CAJERA,IND_NOTA_CRED,IND_EXPORTACION,CENTRO_COSTO,
       IND_FERIAS,IND_PROVINCIA,CONSUMO, REDONDEO ,CONVENIO,IND_FACT_TEXTO,IMP_FACT_DESC,IND_GUIA_TEXTO,
       EXCL_AUX,IMPUESTO_FLETE,ON_LINE,CONT_NETO,IND_FMULTIPLE,IND_NC_FICTA,IND_PROMARG,OPER_EXONERADAS,
       OPER_GRATUITAS,OPER_GRAVADAS,OPER_INAFECTAS,MOT_CONTING,TIPO_OPERACION,EST_RES_CON,DETRACCION,PORC_DETRAC
    FROM FACTU.ARFAFE
    WHERE NO_CIA  = p_cNoCia
    AND  TIPO_DOC = p_cTipoDoc
    AND NO_FACTU  = p_cNoFactu;
    
 CURSOR C_ARFAFL IS
   SELECT NO_CIA,TIPO_DOC,NO_FACTU,BODEGA,NO_ARTI,LOTE,CONSECUTIVO,CANTIDAD_FACT,
       CANTIDAD_ENTR,CANTIDAD_DEV,TIPO_PRECIO,PRECIO_UNIT_ORIG,TOTAL,PRECIO_UNIT,
       IND_PARENTESCO,DSCTO_CLIENTE,IGV,P_DSCTO3,M_DSCTO1,M_DSCTO2,M_DSCTO3,
       IMP_IGV,IND_LIN,PRECIO_SIGV,TOTAL_LIN,TIPO_DOC_ALM,NO_DOCU_ALM,TIPO_BS,
       OPER_EXONERADAS,OPER_GRATUITAS ,OPER_GRAVADAS,OPER_INAFECTAS,TIPO_AFECTACION,
       PREC_IGV,MEDIDA,ICBPER
   FROM FACTU.ARFAFL
   WHERE NO_CIA  = p_cNoCia
   AND TIPO_DOC = p_cTipoDoc
   AND NO_FACTU  = p_cNoFactu;
   
   
 CURSOR C_Artstrd_PVen IS
   SELECT NO_CIA,TIPO_DOC,NO_FACTU,CLASE_TRANSC,TIPO_M,TIPO_ACC,
       COD_ENTIDAD,FECHA,IMPORTE,TIPO_CABA_ORI,COD_CABA_ORI,TIPO_CABA_DES,
       TIPO_OPER,COD_OPER,NO_LINEA,IMP_SOL
   FROM Artstrd_PVen
   WHERE NO_CIA = p_cNoCia
   AND TIPO_DOC = p_cTipoDoc
   AND NO_FACTU = p_cNoFactu;
   
 --
 U C_ARFAFE%ROWTYPE;
 D C_ARFAFL%ROWTYPE;
 T C_Artstrd_PVen%ROWTYPE;
 --
 cNoFactu   FACTU.ARFAFE.NO_FACTU%TYPE;
 nConsDesde FACTU.ARFACC.CONS_DESDE%TYPE;
 --
 nCorrelFict FACTU.ARFACF.CORREL_FICT%TYPE;
 cNoGuia     FACTU.ARFAFE.NO_GUIA%TYPE;
 --
 cRuc CXC.ARCCMC.RUC%TYPE;
 cNuDocumento CXC.ARCCMC.NU_DOCUMENTO%TYPE;
 cTipoDocumento CXC.ARCCMC.TIPO_DOCUMENTO%TYPE;
 --
 cTipoDocCli FACTU.ARFAFE.TIPO_DOC_CLI%TYPE;
 cNumDocCli FACTU.ARFAFE.NUM_DOC_CLI%TYPE;
  
 BEGIN
    p_cError := '0,OK';
    
    IF p_cNoCia IS NULL THEN
        p_cError := '1,Codigo de compañia no puede ser nula';
        RETURN;
    END IF;
      --
    IF p_cTipoDoc IS NULL THEN
        p_cError := '1,Tipo de documento no puede ser nula';
        RETURN;
    END IF;
      --
    IF p_cNoFactu IS NULL THEN
        p_cError := '1,El numero de factura no puede ser nulo';
        RETURN;
    END IF;
      --
    
   dbms_output.put_line('INICIO DUPLICADO_FACTURA');
   dbms_output.put_line('p_cNoCia = '||p_cNoCia||' p_cTipoDoc '||p_cTipoDoc||' p_cNoFactu = '||p_cNoFactu);
   
   i := 1;
   dbms_output.put_line('OBTENEMOS EL NUEVO CORRELATIVO DE LA FACTURA');
   BEGIN
        
        nConsDesde := FACTU.PR_DOCUMENTO.OBTENER_CORRELATIVO(P_cNoCia,P_cCentro,P_cTipoDoc,P_cSerie);
        
        IF nConsDesde < 0 THEN
           P_cError := '1,El documento : '||P_cTipoDoc||' no tiene correlativo.';
           RETURN;
        END IF;
        
        nConsDesde := nConsDesde + 1; 
        
        dbms_output.put_line('MAX. NUMERO DEL CORRELATIVO DE LA FACTURA = '||nConsDesde);
        
        cNoFactu := p_cSerie||LPAD( nConsDesde ,7,'0');
        
        dbms_output.put_line('NEW NUMERO DE LA FACTURA = '||cNoFactu);
   
   EXCEPTION
     WHEN OTHERS THEN
       dbms_output.put_line('No tiene configurado el correlativo en la tabla FACTU.ARFACC para los parametros ');
       p_cError := '1,No tiene configurado el correlativo en la tabla FACTU.ARFACC para los parametros : p_cNoCia = '||p_cNoCia||' p_cCentro = '||p_cCentro||' p_cTipoDoc = '||p_cTipoDoc||' p_cSerie = '||p_cSerie;
       RETURN;
   END;
   
   dbms_output.put_line('INICIAMOS CANTIDAD DE COPIAS');
   LOOP
     
      EXIT WHEN i > p_nNumCopia;
      OPEN C_ARFAFE;
      LOOP
         i := i + 1;
         U := NULL;
         FETCH C_ARFAFE INTO U;
         EXIT WHEN C_ARFAFE%NOTFOUND;
                  
          dbms_output.put_line('INSERTA FACTU.ARFAFE '||i);
          
          BEGIN
            SELECT nvl(CORREL_FICT,0)+1 , SERIE_GR||LPAD(nvl(CORREL_FICT,0)+1,7,'0')
            INTO  nCorrelFict, cNoGuia
            FROM FACTU.ARFACF
            WHERE no_cia  = p_cNoCia
            AND CENTRO    = U.CENTRO;
          EXCEPTION
            WHEN OTHERS THEN
               p_cError := '1,ERROR CONSULTAR LA GUIA FICTAS FACTU.ARFACF - '||SQLERRM;
               RETURN;
          END;
          
          dbms_output.put_line('GUIA FICTAS ES =  '||cNoGuia);
          
          BEGIN
            SELECT RUC, NU_DOCUMENTO, TIPO_DOCUMENTO
            INTO cRuc, cNuDocumento, cTipoDocumento
            FROM CXC.ARCCMC C
            WHERE C.NO_CIA = p_cNoCia
            AND C.NO_CLIENTE = p_cNoCliente;
          EXCEPTION
            WHEN OTHERS THEN
               cRuc := NULL;
               cNuDocumento := NULL;
               cTipoDocumento := NULL;
          END;
          
          IF p_cTipoDoc = 'F' THEN
             cNumDocCli := cRuc;
          ELSE
             cNumDocCli := cNuDocumento;
          END IF;
          
         BEGIN 
          
          INSERT INTO FACTU.ARFAFE(
               NO_CIA,TIPO_DOC,NO_FACTU,GRUPO,NO_CLIENTE,CENTRO,BODEGA,FECHA,TIPO_CLIENTE,
               NBR_CLIENTE,NO_VENDEDOR,TIPO_PRECIO,MONEDA,TIPO_CAMBIO,NO_ORDEN,SUB_TOTAL,IMPUESTO,
               TOTAL,ESTADO,COD_FPAGO,NO_GUIA,TIPO,DIVISION,FECHA_ENTREGA,MOTIVO_TRASLADO,RUC,IND_DOC,
               COD_T_PED,COD_CLAS_PED,TIPO_FPAGO,VALOR_VENTA,M_DSCTO_GLOBAL,FECHA_VENCE,CODI_DEPA,CODI_PROV,
               CODI_DIST,NO_DOCU,COD_TIENDA,COD_DIR_ENTREGA,TIPO_DOC_CLI,NUM_DOC_CLI,IMPRIME,IND_VTA_ANTICIPADA,
               TOTAL_BRUTO,T_DESCUENTO,IND_PVENT,COD_CAJA,CAJERA,IND_NOTA_CRED,IND_EXPORTACION,CENTRO_COSTO,
               IND_FERIAS,IND_PROVINCIA,CONSUMO, REDONDEO ,CONVENIO,IND_FACT_TEXTO,IMP_FACT_DESC,IND_GUIA_TEXTO,
               EXCL_AUX,IMPUESTO_FLETE,ON_LINE,CONT_NETO,IND_FMULTIPLE,IND_NC_FICTA,IND_PROMARG,OPER_EXONERADAS,
               OPER_GRATUITAS,OPER_GRAVADAS,OPER_INAFECTAS,MOT_CONTING,TIPO_OPERACION,EST_RES_CON,DETRACCION,PORC_DETRAC
          )
          VALUES (
               p_cNoCia,p_cTipoDoc,cNoFactu,U.GRUPO,p_cNoCliente,U.CENTRO,U.BODEGA,p_dFecha,U.TIPO_CLIENTE,
               U.NBR_CLIENTE,U.NO_VENDEDOR,U.TIPO_PRECIO,U.MONEDA,U.TIPO_CAMBIO,p_cNoOrden ,U.SUB_TOTAL,U.IMPUESTO,
               U.TOTAL,U.ESTADO,U.COD_FPAGO,cNoGuia,U.TIPO,U.DIVISION,p_dFecha,U.MOTIVO_TRASLADO,cRuc,U.IND_DOC,
               U.COD_T_PED,U.COD_CLAS_PED,U.TIPO_FPAGO,U.VALOR_VENTA,U.M_DSCTO_GLOBAL,p_dFecha,U.CODI_DEPA,U.CODI_PROV,
               U.CODI_DIST,U.NO_DOCU,U.COD_TIENDA,U.COD_DIR_ENTREGA,cTipoDocCli,cNumDocCli,U.IMPRIME,U.IND_VTA_ANTICIPADA,
               U.TOTAL_BRUTO,U.T_DESCUENTO,U.IND_PVENT,U.COD_CAJA,U.CAJERA,U.IND_NOTA_CRED,U.IND_EXPORTACION,U.CENTRO_COSTO,
               U.IND_FERIAS,U.IND_PROVINCIA,U.CONSUMO, U.REDONDEO ,U.CONVENIO,U.IND_FACT_TEXTO,U.IMP_FACT_DESC,U.IND_GUIA_TEXTO,
               U.EXCL_AUX,U.IMPUESTO_FLETE,U.ON_LINE,U.CONT_NETO,U.IND_FMULTIPLE,U.IND_NC_FICTA,U.IND_PROMARG,U.OPER_EXONERADAS,
               U.OPER_GRATUITAS,U.OPER_GRAVADAS,U.OPER_INAFECTAS,U.MOT_CONTING,U.TIPO_OPERACION,U.EST_RES_CON,U.DETRACCION,U.PORC_DETRAC
          );
          
         EXCEPTION
            WHEN OTHERS THEN
               p_cError := '1,ERROR INSERT FACTU.ARFAFE, cNoFactu = '||cNoFactu||' , p_cNoCia = '||p_cNoCia||' , p_cTipoDoc = '||p_cTipoDoc||' - '||SQLERRM;
               RETURN;
         END;                  
         
         dbms_output.put_line('INSERTA FACTU.ARFAFL ');
         
         OPEN C_ARFAFL;
         LOOP
           
           D := NULL;
           FETCH C_ARFAFL INTO D;
           EXIT WHEN C_ARFAFL%NOTFOUND;
           
           BEGIN
              dbms_output.put_line('INSERTA ARFAFL ');
              
              INSERT INTO FACTU.ARFAFL(
                   NO_CIA,TIPO_DOC,NO_FACTU,BODEGA,NO_ARTI,LOTE,CONSECUTIVO,CANTIDAD_FACT,
                   CANTIDAD_ENTR,CANTIDAD_DEV,TIPO_PRECIO,PRECIO_UNIT_ORIG,TOTAL,PRECIO_UNIT,
                   IND_PARENTESCO,DSCTO_CLIENTE,IGV,P_DSCTO3,M_DSCTO1,M_DSCTO2,M_DSCTO3,
                   IMP_IGV,IND_LIN,PRECIO_SIGV,TOTAL_LIN,TIPO_DOC_ALM,NO_DOCU_ALM,TIPO_BS,
                   OPER_EXONERADAS,OPER_GRATUITAS ,OPER_GRAVADAS,OPER_INAFECTAS,TIPO_AFECTACION,
                   PREC_IGV,MEDIDA,ICBPER)
              VALUES (
                   p_cNoCia,p_cTipoDoc,cNoFactu,D.BODEGA,D.NO_ARTI,D.LOTE,D.CONSECUTIVO,D.CANTIDAD_FACT,
                   D.CANTIDAD_ENTR,D.CANTIDAD_DEV,D.TIPO_PRECIO,D.PRECIO_UNIT_ORIG,D.TOTAL,D.PRECIO_UNIT,
                   D.IND_PARENTESCO,D.DSCTO_CLIENTE,D.IGV,D.P_DSCTO3,D.M_DSCTO1,D.M_DSCTO2,D.M_DSCTO3,
                   D.IMP_IGV,D.IND_LIN,D.PRECIO_SIGV,D.TOTAL_LIN,D.TIPO_DOC_ALM,D.NO_DOCU_ALM,D.TIPO_BS,
                   D.OPER_EXONERADAS,D.OPER_GRATUITAS ,D.OPER_GRAVADAS,D.OPER_INAFECTAS,D.TIPO_AFECTACION,
                   D.PREC_IGV,D.MEDIDA,D.ICBPER);
              
           EXCEPTION
                WHEN OTHERS THEN
                   p_cError := '1,ERROR INSERT ARFAFL - '||SQLERRM;
                   RETURN;
           END;                      
           
         END LOOP;
         
         CLOSE C_ARFAFL;
         
         OPEN C_Artstrd_PVen;
         LOOP
             T := NULL;
             FETCH C_Artstrd_PVen INTO T;
             EXIT WHEN C_Artstrd_PVen%NOTFOUND;
             
             BEGIN
                  dbms_output.put_line('INSERTA Artstrd_PVen ');
                  
                  INSERT INTO Artstrd_PVen(
                       NO_CIA,TIPO_DOC,NO_FACTU,CLASE_TRANSC,TIPO_M,TIPO_ACC,
                       COD_ENTIDAD,FECHA,IMPORTE,TIPO_CABA_ORI,COD_CABA_ORI,TIPO_CABA_DES,
                       TIPO_OPER,COD_OPER,NO_LINEA,IMP_SOL)
                  VALUES (
                       p_cNoCia,p_cTipoDoc ,cNoFactu,T.CLASE_TRANSC,T.TIPO_M,T.TIPO_ACC,
                       T.COD_ENTIDAD,p_dFecha,T.IMPORTE,T.TIPO_CABA_ORI,T.COD_CABA_ORI,T.TIPO_CABA_DES,
                       T.TIPO_OPER,T.COD_OPER,T.NO_LINEA,T.IMP_SOL);
                  
             EXCEPTION
                    WHEN OTHERS THEN
                       p_cError := '1,ERROR INSERT Artstrd_PVen - '||SQLERRM;
                       RETURN;
             END;
                          
         END LOOP;
           
         CLOSE C_Artstrd_PVen;
         
         -- VAMOS ACTUALIZAR EL CORRELATIVO DE LA GUIA FICTA
         BEGIN
            UPDATE FACTU.ARFACF
            SET CORREL_FICT = nCorrelFict
            WHERE no_cia  = p_cNoCia
            AND CENTRO    = U.CENTRO;
         EXCEPTION
            WHEN OTHERS THEN
               p_cError := '1,ERROR UPDATE LA GUIA FICTAS FACTU.ARFACF - '||SQLERRM;
               RETURN;
         END;   
         
      END LOOP;
      
      CLOSE C_ARFAFE;
      
      -- VAMOS ACTUALIZAR EL CORRELATIVO DE LA SERIE DE LA FACTURA
      BEGIN            
            UPDATE FACTU.ARFACC
            SET CONS_DESDE = nConsDesde
            WHERE NO_CIA = p_cNoCia
            AND CENTRO   = p_cCentro
            AND TIPO_DOC = p_cTipoDoc
            AND SERIE    = p_cSerie;            
      EXCEPTION
           WHEN OTHERS THEN
               p_cError := '1,ERROR UPDATE FACTU.ARFACC - '||SQLERRM;
               RETURN;
      END;
   
   END LOOP;
   
   
 END DUPLICADO_FACTURA;
 
 
 PROCEDURE COPIA_FACTURA(
        p_cNoCia     IN FACTU.ARFAFE.NO_CIA%TYPE,
        p_cTipoDoc   IN FACTU.ARFACC.TIPO_DOC%TYPE DEFAULT 'F',
        p_cNoFactu   IN FACTU.ARFAFE.NO_FACTU%TYPE,
        p_dFecha     IN FACTU.ARFAFE.FECHA%TYPE DEFAULT SYSDATE,
        p_cNoCliente IN FACTU.ARFAFE.NO_CLIENTE%TYPE,
        p_cGuiaTemp  IN VARCHAR,
        p_nNumCopia  IN NUMBER DEFAULT 1,
        --
        p_cNewNoFactu OUT FACTU.ARFAFE.NO_FACTU%TYPE,    
        p_cError      IN OUT VARCHAR2
        ) IS
   
  cNoOrden    FACTU.ARFAFE.NO_ORDEN%TYPE;
  cNoCliente  FACTU.ARFAFE.NO_CLIENTE%TYPE;
  cNewNoOrden FACTU.ARFAFE.NO_ORDEN%TYPE;
  --
  nConsDesde FACTU.ARFACC.CONS_DESDE%TYPE;
  
  KSerie CONSTANT VARCHAR2(4) := 'F001';
  kCentro CONSTANT VARCHAR2(2) := '41';
 
 BEGIN
 
    dbms_output.put_line(' INICIO PR_FACTURA.COPIA_FACTURA ');
    dbms_output.put_line(' =================================================================================== ');
   
    IF p_cNoCia IS NULL THEN
        p_cError := '1,Código de compañia no puede ser nula';
        RETURN;
    END IF;
    
    IF p_cNoFactu IS NULL THEN
        p_cError := '1,Número de factura no puede ser nula';
        RETURN;
    END IF;
    
    BEGIN
      SELECT NO_ORDEN,NO_CLIENTE
      INTO cNoOrden,cNoCliente
      FROM FACTU.ARFAFE F
      WHERE NO_CIA = p_cNoCia
      AND NO_FACTU = p_cNoFactu;
    EXCEPTION
       WHEN OTHERS THEN
         p_cError := '1,Numero de factura = '||p_cNoFactu||' y compañia '|| p_cNoCia ||' no valido';
         RETURN;
    END;
    
    IF p_cNoCliente IS NOT NULL THEN
        cNoCliente := p_cNoCliente;
    END IF;
    
    dbms_output.put_line(' INICIO PR_PEDIDO.COPIA_PEDIDO ');
    dbms_output.put_line(' =================================================================================== ');
    
    -- FACTU.PR_PEDIDO.COPIA_PEDIDO(p_cNoCia,cNoOrden,cNoCliente,p_dFecha,p_cGuiaTemp,p_nNumCopia,kCentro,'P',KSerie,cNewNoOrden,p_cError );
    FACTU.PR_PEDIDO.COPIA_PEDIDO(p_cNoCia,cNoOrden,cNoCliente,p_dFecha,p_cGuiaTemp,p_nNumCopia,kCentro,'P','941',cNewNoOrden,p_cError );
   
    dbms_output.put_line('NEW PEDIDO = '||cNewNoOrden);
    
    BEGIN
      
      UPDATE FACTU.ARPFOE
      SET ESTADO = 'C'
      WHERE NO_CIA = p_cNoCia
      AND NO_ORDEN = cNewNoOrden;
    
    EXCEPTION
      WHEN OTHERS THEN
         p_cError := '1,Error para actualizar el estado al nuevo pedido, p_cNoCia = '||p_cNoCia||' y cNewNoOrden =  '|| cNewNoOrden;
         RETURN;
    END;
    
    dbms_output.put_line(' INICIO PR_FACTURA.DUPLICADO_FACTURA ');
    dbms_output.put_line(' =================================================================================== ');
  
    IF P_cError = '0,OK' THEN
        
        PR_FACTURA.DUPLICADO_FACTURA(p_cNoCia,p_cTipoDoc,p_cNoFactu, cNewNoOrden, p_dFecha,cNoCliente,p_nNumCopia, kCentro,KSerie,p_cError);
        
        IF P_cError = '0,OK' THEN
           /*
           BEGIN
                SELECT CONS_DESDE
                INTO nConsDesde
                FROM FACTU.ARFACC
                WHERE NO_CIA = p_cNoCia
                AND CENTRO   = '41'
                AND TIPO_DOC = P_cTipoDoc
                AND SERIE    = 'F001';                
                          
                p_cNewNoFactu := 'F001'||LPAD(nConsDesde ,7,'0');
                
                dbms_output.put_line('SE CREO LA NUEVA FACTURA = '||p_cNewNoFactu);
           EXCEPTION
               WHEN OTHERS THEN
                 p_cError := '1,Error para obtener el nuevo numero de la factura = '||p_cNewNoFactu||' y P_cTipoDoc =  '|| P_cTipoDoc ||' , p_cNoCia = '||p_cNoCia;
                 RETURN;                
           END;
           */
            nConsDesde := FACTU.PR_DOCUMENTO.OBTENER_CORRELATIVO(P_cNoCia,kCentro,P_cTipoDoc,KSerie);
        
            IF nConsDesde < 0 THEN
               P_cError := '1,El documento : '||P_cTipoDoc||' no tiene correlativo.';
               RETURN;
            END IF;
            
            p_cNewNoFactu := KSerie||LPAD(nConsDesde ,7,'0');
            
            IF NVL(p_cNewNoFactu,'X') != 'X' THEN
                CREAR_ARCHIVO_SFS(p_cNoCia, p_cTipoDoc, p_cNewNoFactu);
            END IF; 
           
        END IF;
      
    END IF;    
 
 END COPIA_FACTURA;
 
   /*---------------------------------------------------------------------------------------
   Nombre      : MIGRAR_PEDIDO
   Proposito   : PROCEDIMIENTO QUE NOS PERMITE MIGRAR DE FACTU.TDU_TABLA_ARPFOE A FACTU.TDU_TABLA_ARFAFE
   Parametro  :
              p_tdu_arpfoe   FACTU.TDU_TABLA_ARPFOE
              p_tdu_arfafe   FACTU.TDU_TABLA_ARFAFA
             p_cError        VARCHAR2

   Log de Cambios:
     Fecha        Autor                     Descripción
     11/09/2024   Robinzon Santana          Creador   
  -----------------------------------------------------------------------------------------*/
  PROCEDURE MIGRAR_PEDIDO(p_tdu_arpfoe IN FACTU.TDU_TABLA_ARPFOE, 
                          p_tdu_arfafe IN OUT FACTU.TDU_TABLA_ARFAFE,
                          --
                          p_cError IN OUT VARCHAR2
                          ) IS
  BEGIN
    p_cError := '0,OK';
    
    p_tdu_arfafe := FACTU.TDU_TABLA_ARFAFE();
    
    FOR i IN 1..p_tdu_arpfoe.COUNT LOOP
      p_tdu_arfafe.EXTEND;
      -- p_tdu_arfafe(p_tdu_arfafe.LAST) := OBJ_ARFAFE(p_tdu_arpfoe(i).NO_CIA);
    END LOOP;
    
  END MIGRAR_PEDIDO;
  
  /*---------------------------------------------------------------------------------------
   Nombre      : SET_TDU_ARFAFE
   Proposito   : PROCESO QUE NOS PERMITE OBTENER EL ARRAY DE ARFAFE
   Parametro  :
             

   Log de Cambios:
     Fecha        Autor                     Descripción
     10/10/2024   Robinzon Santana          Creador   
  -----------------------------------------------------------------------------------------*/
  PROCEDURE SET_TDU_ARFAFE(
                           p_cNoCia IN FACTU.ARPFOE.NO_CIA%TYPE,
                           p_cNoOrder IN FACTU.ARPFOE.NO_ORDEN%TYPE,
                           p_tEmiCp IN FACTU.TDU_TABLA_EMI_CP,
                           p_cNoGuia IN FACTU.ARFAFE.NO_GUIA%TYPE,
                           p_nNoDocu IN INVE.ARINSE.SECUENCIA%TYPE,
                           p_cError IN OUT VARCHAR2,
                           p_tArfafe OUT FACTU.TDU_TABLA_ARFAFE,
                           p_cNoFactu OUT FACTU.ARFAFE.NO_FACTU%TYPE
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
        WHERE NO_CIA = p_cNoCia
        AND NO_ORDEN = p_cNoOrder;        
    
    cTipoDoc VARCHAR2(2);
    cTipoCliente VARCHAR2(2);
    nTipoCambio NUMBER(8,4);
    cIndDoc VARCHAR2(1);
    nMDsctoGlobal NUMBER(11,3);
    --
    cNoFactu FACTU.ARFAFE.NO_FACTU%TYPE;
    nTotal FACTU.ARFAFE.TOTAL%TYPE;
    --
    cCodiDepa CXC.ARCCTDA.CODI_DEPA%TYPE;
    cCodiProv CXC.ARCCTDA.CODI_PROV%TYPE;
    cCodiDist CXC.ARCCTDA.CODI_DIST%TYPE;
    --
         
  BEGIN
     DBMS_OUTPUT.PUT_LINE('ENTRO  FACTU.SET_TDU_ARFAFE ');
     
     FOR i IN 1..p_tEmiCp.COUNT LOOP
        cTipoDoc := p_tEmiCp(i).tipoDoc;
        cTipoCliente := p_tEmiCp(i).tipoCliente;
        nTipoCambio := p_tEmiCp(i).tipoCambio;
        cIndDoc := p_tEmiCp(i).indDoc;
        nMDsctoGlobal := p_tEmiCp(i).mDsctoGlobal;
     END LOOP;
     
     p_tArfafe := FACTU.TDU_TABLA_ARFAFE();
     
     <<factura>>
     FOR i IN C_ARPFOE LOOP
     
        p_cNoFactu := FACTU.PR_DOCUMENTO.GET_NUM_DOCUMENTO(i.NO_CIA, i.CENTRO, cTipoDoc, p_cError);
        
        BEGIN
            SELECT CODI_DEPA, CODI_PROV, CODI_DIST
            INTO cCodiDepa, cCodiProv, cCodiDist
            FROM CXC.ARCCTDA
            WHERE NO_CIA = i.NO_CIA 
            AND NO_CLIENTE = i.NO_CLIENTE 
            AND COD_TIENDA = '001';
        EXCEPTION
          WHEN OTHERS THEN
             p_cError := '1,EL CLIENTE '||i.NO_CLIENTE||' NO TIENE DIRECCIÓN REGISTRADA.';
        END;
     
        IF p_cError != '0,OK' THEN
           DBMS_OUTPUT.PUT_LINE(p_cError);
           EXIT factura;
        END IF;
        
        nTotal := NVL(i.SUB_TOTAL, 0) + NVL(i.T_IMPUESTO,0);
        
        p_tArfafe.EXTEND;
        
        p_tArfafe(p_tArfafe.LAST) := FACTU.OBJ_ARFAFE(i.NO_CIA,cTipoDoc, p_cNoFactu,'00', i.NO_CLIENTE, i.CENTRO, i.BODEGA, i.FECHA_REGISTRO, cTipoCliente, i.NOMBRE_CLIENTE,
                         i.NO_VENDEDOR, i.TIPO_PRECIO, i.MONEDA, nTipoCambio, i.NO_ORDEN, i.SUB_TOTAL, i.T_IMPUESTO, nTotal, 'D', i.COD_FPAGO,
                         p_cNoGuia, i.TIPO, i.DIVISION, i.FECHA_ENTREGA, i.MOTIVO_TRASLADO, i.RUC, cIndDoc, i.COD_T_PED, i.COD_CLAS_PED, i.TIPO_FPAGO,
                         i.T_VALOR_VENTA, nMDsctoGlobal, i.FECHA_VENCE, cCodiDepa, cCodiProv, cCodiDist, p_nNoDocu, i.COD_TIENDA, i.COD_DIR_ENTREGA, i.TIPO_DOC_CLI,
                         i.NUM_DOC_CLI,'S', i.IND_VTA_ANTICIPADA, i.TOTAL_BRUTO, i.T_DESCUENTO, i.IND_PVENT, i.COD_CAJA, i.CAJERA, i.IND_NOTA_CRED, i.IND_EXPORTACION,
                         i.CENTRO_COSTO, i.IND_FERIAS, i.IND_PROVINCIA, i.CONSUMO, i.REDONDEO, i.CONVENIO, i.IND_FACT_TEXTO, 'N', i.IND_GUIA_TEXTO, 'N',
                         i.IMPUESTO_FLETE, i.ON_LINE, i.CONT_NETO, 'N', 'N', 'N', i.OPER_EXONERADAS, i.OPER_GRATUITAS, i.OPER_GRAVADAS, 0,
                         i.MOT_CONTING, i.TIPO_OPERACION, 'N', 0, 0);
        
     END LOOP;
     
  EXCEPTION
    WHEN OTHERS THEN
       p_cError := '1,'|| SQLERRM;     
  END SET_TDU_ARFAFE;
  
 /*----------------------------------------------------------------------
   Nombre      : SET_TDU_ARFAFL
   Proposito   : PROCESO QUE NOS PERMITE OBTENER EL ARRAY DE ARFAFL
   Parametro  :
             

   Log de Cambios:
     Fecha        Autor                     Descripción
     16/10/2024   Robinzon Santana          Creador   
 -----------------------------------------------------------------------*/
  PROCEDURE SET_TDU_ARFAFL(p_cNoCia IN FACTU.ARPFOE.NO_CIA%TYPE,
                           p_cNoOrder IN FACTU.ARPFOE.NO_ORDEN%TYPE,
                           p_nNoDocu IN INVE.ARINSE.SECUENCIA%TYPE,
                           p_cNoFactu IN FACTU.ARFAFE.NO_FACTU%TYPE,                          
                           p_tArfafl IN OUT NOCOPY FACTU.TDU_TABLA_ARFAFL,
                           p_cError IN OUT VARCHAR2
                          ) IS
                          
      CURSOR C_ARPFOL IS
        SELECT NO_CIA,NO_ORDEN,GRUPO,NO_CLIENTE,NO_ARTI,TIPO_ARTI,ARTI_NUEVO,BODEGA,CANT_COMP,CANT_SOLICITADA,
         CANT_ENTREG,CANT_ASIGNADA,CANT_REASIGNADA,FECHA_REGISTRO,PRECIO,TOT_LINEA,ESTADO,DSCTO_CLIENTE,D_PROMO,
         IGV,NO_LINEA,P_DSCTO3,M_DSCTO2,M_DSCTO3,IMP_IGV,PRECIO_SIGV,TOTAL_LIN,DESCRIPCION,PARTE,TIPO_BS,IND_PIDE_LOTE,
         OPER_EXONERADAS,OPER_GRATUITAS,OPER_GRAVADAS,OPER_INAFECTAS,TIPO_AFECTACION,PREC_IGV,MEDIDA
        FROM FACTU.ARPFOL
        WHERE NO_CIA = p_cNoCia
        AND NO_ORDEN = p_cNoOrder;
        
   cTipoDoc FACTU.ARFAFL.TIPO_DOC%TYPE;
   klote FACTU.ARFAFL.LOTE%TYPE := TO_DATE('01/01/2070','DD/MM/YYYY');
   
   cTipoTransc FACTU.ARPFFE.TIPO_TRANSC%TYPE;
                          
  BEGIN        
     DBMS_OUTPUT.PUT_LINE('>>>ENTRO PR_FACTU.SET_TDU_ARFAFL');
     DBMS_OUTPUT.PUT_LINE('>>> p_cNoCia '||p_cNoCia||' , p_cNoOrder '||p_cNoOrder||' , p_nNoDocu '||p_nNoDocu||' , p_cNoFactu '||p_cNoFactu);
     BEGIN
         SELECT TIPO_TRANSC
         INTO cTipoTransc
         FROM FACTU.ARPFFE
         WHERE NO_CIA = p_cNoCia
         AND NO_ORDEN = p_cNoOrder;
     EXCEPTION
       WHEN OTHERS THEN
          DBMS_OUTPUT.PUT_LINE('>>> NO TRAE DATOS FACTU.ARPFFE');
          cTipoTransc := '1315';
     END;
     
     cTipoDoc := SUBSTR(p_cNoFactu, 1, 1);
     
     p_tArfafl := FACTU.TDU_TABLA_ARFAFL();
     
     FOR i IN C_ARPFOL LOOP             
        -- DBMS_OUTPUT.PUT_LINE('>>> SET_TDU_ARFAFL => i.NO_CIA = '||i.NO_CIA||' , cTipoDoc = '||cTipoDoc||' , p_cNoFactu = '||p_cNoFactu);
        p_tArfafl.EXTEND;
        
        p_tArfafl(p_tArfafl.LAST) := FACTU.OBJ_ARFAFL(NO_CIA => p_cNoCia, 
                                                      TIPO_DOC => cTipoDoc, 
                                                      NO_FACTU => p_cNoFactu, 
                                                      BODEGA => i.BODEGA, 
                                                      NO_ARTI => i.NO_ARTI, 
                                                      LOTE => NULL, 
                                                      CONSECUTIVO => i.NO_LINEA, 
                                                      CANTIDAD_FACT => i.CANT_ASIGNADA,
                                                      CANTIDAD_ENTR => i.CANT_ASIGNADA,
                                                      CANTIDAD_DEV => i.CANT_ASIGNADA,
                                                      TIPO_PRECIO => 'A3',
                                                      PRECIO_UNIT_ORIG => i.PRECIO,
                                                      TOTAL => i.TOT_LINEA,
                                                      PRECIO_UNIT => i.PRECIO,
                                                      IND_PARENTESCO => 'N',
                                                      DSCTO_CLIENTE => i.DSCTO_CLIENTE,
                                                      IGV => i.IGV,
                                                      P_DSCTO3 => i.P_DSCTO3,
                                                      M_DSCTO1 => i.P_DSCTO3,
                                                      M_DSCTO2 => i.P_DSCTO3,
                                                      M_DSCTO3 => i.P_DSCTO3,
                                                      IMP_IGV => i.IMP_IGV,
                                                      IND_LIN => 'N',
                                                      PRECIO_SIGV => i.PRECIO_SIGV,
                                                      TOTAL_LIN => i.TOTAL_LIN,
                                                      TIPO_DOC_ALM => cTipoTransc,
                                                      NO_DOCU_ALM => p_nNoDocu,
                                                      TIPO_BS => 'L',
                                                      OPER_EXONERADAS => i.OPER_EXONERADAS,
                                                      OPER_GRATUITAS => i.OPER_GRATUITAS,
                                                      OPER_GRAVADAS => i.OPER_GRAVADAS,
                                                      OPER_INAFECTAS => i.OPER_INAFECTAS,
                                                      TIPO_AFECTACION => i.TIPO_AFECTACION,
                                                      PREC_IGV => i.PREC_IGV,
                                                      MEDIDA => i.MEDIDA,
                                                      ICBPER => 0);
     
     END LOOP;
  EXCEPTION
    WHEN OTHERS THEN
       p_cError := '1,'|| SQLERRM;          
  END SET_TDU_ARFAFL;
  
 /*----------------------------------------------------------------
   Nombre      : GUARDAR
   Proposito   :  PROCESO QUE NOS VA PERMITIR CREAR UNA FACTURA O BOLETA
   Parametro  :

   Log de Cambios:
     Fecha        Autor                     Descripción
     17/10/2024   Robinzon Santana          Creador
    03/04/2025   Robinzon Santana           Modificando fecha de creacion por la fecha actual
    04/02/2026   Robinzon Santana           <R-03> Agregando envio a SUNAT
  --------------------------------------------------------------------*/
  PROCEDURE GUARDAR(p_cNoCia IN FACTU.ARPFOE.NO_CIA%TYPE,
                    p_cNoOrder IN FACTU.ARPFOE.NO_ORDEN%TYPE,
                    p_cNoGuia IN FACTU.ARFAFE.NO_GUIA%TYPE,
                    p_nNoDocu IN INVE.ARINSE.SECUENCIA%TYPE,
                    p_tEmiCp IN FACTU.TDU_TABLA_EMI_CP,
                    p_cNoFactu OUT FACTU.ARFAFE.NO_FACTU%TYPE,
                    p_cError IN OUT VARCHAR2) IS
    
  tArfafe FACTU.TDU_TABLA_ARFAFE;
  tArfafl FACTU.TDU_TABLA_ARFAFL;
  cNoFactu FACTU.ARFAFE.NO_FACTU%TYPE;
  cSerie FACTU.ARFACC.SERIE%TYPE;
  cBodega FACTU.ARFAFE.BODEGA%TYPE;
  cTipoDoc FACTU.ARFAFE.TIPO_DOC%TYPE;
  --
  nCantidad integer(5);
  nCantArfafl integer(5);
   --
 cRuc CXC.ARCCMC.RUC%TYPE;
 cNuDocumento CXC.ARCCMC.NU_DOCUMENTO%TYPE;
 cTipoDocumento CXC.ARCCMC.TIPO_DOCUMENTO%TYPE;
 --
 cTipoDocCli FACTU.ARFAFE.TIPO_DOC_CLI%TYPE;
 cNumDocCli FACTU.ARFAFE.NUM_DOC_CLI%TYPE;
 cTipoCliente FACTU.ARFAFE.TIPO_CLIENTE%TYPE;
 cGuiaTemp FACTU.ARPFOE.GUIA_TEMP%TYPE;
 --
 cEnvAws FACTU.ARFAFE.ENVIAWS%TYPE; -- <R-03>
 
 BEGIN
    DBMS_OUTPUT.PUT_LINE('>>> ENTRO PR_FACTU.GUARDAR');
    FACTU.PR_FACTURA.SET_TDU_ARFAFE(p_cNoCia, p_cNoOrder, p_tEmiCp, p_cNoGuia, p_nNoDocu, p_cError, tArfafe, cNoFactu);
    DBMS_OUTPUT.PUT_LINE('>>> ENTRO PR_FACTU.SET_TDU_ARFAFE => '||p_cError);
    -- GUARDAR FACTU.ARFAFE
    IF p_cError = '0,OK' THEN
       p_cNoFactu := cNoFactu;
       BEGIN
         nCantidad := tArfafe.COUNT;
       EXCEPTION
         WHEN OTHERS THEN
            nCantidad := 0;  
       END;
       
       DBMS_OUTPUT.PUT_LINE('>>> ENTRO PR_FACTU.SET_TDU_ARFAFE => CANTIDAD = '||nCantidad);
       
       IF nCantidad > 0 THEN
          
         FOR i IN 1..nCantidad LOOP
         
             cBodega := tArfafe(i).BODEGA;
             cTipoDoc := tArfafe(i).TIPO_DOC;
             DBMS_OUTPUT.PUT_LINE('>>> tArfafe(i).NO_CIA = '||tArfafe(i).NO_CIA);
             DBMS_OUTPUT.PUT_LINE('>>> tArfafe(i).TIPO_DOC = '||tArfafe(i).TIPO_DOC);
             DBMS_OUTPUT.PUT_LINE('>>> tArfafe(i).NO_FACTU = '||tArfafe(i).NO_FACTU);
             
             BEGIN
                SELECT RUC, NU_DOCUMENTO, TIPO_DOCUMENTO, TIPO_CLIENTE
                INTO cRuc, cNuDocumento, cTipoDocumento, cTipoCliente
                FROM CXC.ARCCMC C
                WHERE C.NO_CIA = tArfafe(i).NO_CIA
                AND C.NO_CLIENTE = tArfafe(i).NO_CLIENTE;
             EXCEPTION
                WHEN OTHERS THEN
                   cRuc := NULL;
                   cNuDocumento := NULL;
                   cTipoDocumento := NULL;
             END;
              
             IF tArfafe(i).TIPO_DOC = 'F' THEN
                 cNumDocCli := cRuc;
             ELSE
                 cNumDocCli := cNuDocumento;
             END IF;
             
             IF cTipoCliente = 'V' THEN
                cRuc := NULL;
             END IF;
             
             BEGIN
               SELECT GUIA_TEMP
               INTO cGuiaTemp
               FROM FACTU.ARPFOE
               WHERE NO_CIA = tArfafe(i).NO_CIA
               AND NO_ORDEN = tArfafe(i).NO_ORDEN;
             EXCEPTION
               WHEN OTHERS THEN
                  cGuiaTemp := NULL;
             END;
             -- <I R-03>
             IF tArfafe(i).TIPO_DOC IN ('F', 'B', 'NC') THEN
                 cEnvAws := 'S';
             ELSE
                cEnvAws := 'N';
             END IF;
             -- <F R-03>
              
             BEGIN
                 INSERT INTO FACTU.ARFAFE(
                       NO_CIA,TIPO_DOC,NO_FACTU,GRUPO,NO_CLIENTE,CENTRO,BODEGA,FECHA,TIPO_CLIENTE,
                       NBR_CLIENTE,NO_VENDEDOR,TIPO_PRECIO,MONEDA,TIPO_CAMBIO,NO_ORDEN,SUB_TOTAL,IMPUESTO,
                       TOTAL,ESTADO,COD_FPAGO,NO_GUIA,TIPO,DIVISION,FECHA_ENTREGA,MOTIVO_TRASLADO,RUC,IND_DOC,
                       COD_T_PED,COD_CLAS_PED,TIPO_FPAGO,VALOR_VENTA,M_DSCTO_GLOBAL,FECHA_VENCE,CODI_DEPA,CODI_PROV,
                       CODI_DIST,NO_DOCU,COD_TIENDA,COD_DIR_ENTREGA,TIPO_DOC_CLI,NUM_DOC_CLI,IMPRIME,IND_VTA_ANTICIPADA,
                       TOTAL_BRUTO,T_DESCUENTO,IND_PVENT,COD_CAJA,CAJERA,IND_NOTA_CRED,IND_EXPORTACION,CENTRO_COSTO,
                       IND_FERIAS,IND_PROVINCIA,CONSUMO, REDONDEO ,CONVENIO,IND_FACT_TEXTO,IMP_FACT_DESC,IND_GUIA_TEXTO,
                       EXCL_AUX,IMPUESTO_FLETE,ON_LINE,CONT_NETO,IND_FMULTIPLE,IND_NC_FICTA,IND_PROMARG,OPER_EXONERADAS,
                       OPER_GRATUITAS,OPER_GRAVADAS,OPER_INAFECTAS,MOT_CONTING,TIPO_OPERACION,EST_RES_CON,DETRACCION,PORC_DETRAC,
                       GUIA_TEMP, ENVIAWS, PROCE_STATUS
                  )
                  VALUES (
                       tArfafe(i).NO_CIA ,tArfafe(i).TIPO_DOC, tArfafe(i).NO_FACTU, tArfafe(i).GRUPO, tArfafe(i).NO_CLIENTE,tArfafe(i).CENTRO,tArfafe(i).BODEGA, /*tArfafe(i).FECHA*/ TRUNC(SYSDATE) , tArfafe(i).TIPO_CLIENTE,
                       tArfafe(i).NBR_CLIENTE,tArfafe(i).NO_VENDEDOR,tArfafe(i).TIPO_PRECIO,tArfafe(i).MONEDA,tArfafe(i).TIPO_CAMBIO,tArfafe(i).NO_ORDEN ,tArfafe(i).SUB_TOTAL,tArfafe(i).IMPUESTO,
                       tArfafe(i).TOTAL,tArfafe(i).ESTADO,tArfafe(i).COD_FPAGO,tArfafe(i).NO_GUIA,tArfafe(i).TIPO,tArfafe(i).DIVISION,/*tArfafe(i).FECHA */ SYSDATE,tArfafe(i).MOTIVO_TRASLADO,cRuc,tArfafe(i).IND_DOC,
                       tArfafe(i).COD_T_PED,tArfafe(i).COD_CLAS_PED,tArfafe(i).TIPO_FPAGO,tArfafe(i).VALOR_VENTA,tArfafe(i).M_DSCTO_GLOBAL, /*tArfafe(i).FECHA */ SYSDATE,tArfafe(i).CODI_DEPA,tArfafe(i).CODI_PROV,
                       tArfafe(i).CODI_DIST,tArfafe(i).NO_DOCU,tArfafe(i).COD_TIENDA,tArfafe(i).COD_DIR_ENTREGA,cTipoDocumento,cNumDocCli,tArfafe(i).IMPRIME,tArfafe(i).IND_VTA_ANTICIPADA,
                       tArfafe(i).TOTAL_BRUTO,tArfafe(i).T_DESCUENTO,tArfafe(i).IND_PVENT,tArfafe(i).COD_CAJA,tArfafe(i).CAJERA,tArfafe(i).IND_NOTA_CRED,tArfafe(i).IND_EXPORTACION,tArfafe(i).CENTRO_COSTO,
                       tArfafe(i).IND_FERIAS,tArfafe(i).IND_PROVINCIA,tArfafe(i).CONSUMO, tArfafe(i).REDONDEO ,tArfafe(i).CONVENIO,tArfafe(i).IND_FACT_TEXTO,tArfafe(i).IMP_FACT_DESC,tArfafe(i).IND_GUIA_TEXTO,
                       tArfafe(i).EXCL_AUX,tArfafe(i).IMPUESTO_FLETE,tArfafe(i).ON_LINE,tArfafe(i).CONT_NETO,tArfafe(i).IND_FMULTIPLE,tArfafe(i).IND_NC_FICTA,tArfafe(i).IND_PROMARG,tArfafe(i).OPER_EXONERADAS,
                       tArfafe(i).OPER_GRATUITAS,tArfafe(i).OPER_GRAVADAS,tArfafe(i).OPER_INAFECTAS,tArfafe(i).MOT_CONTING,tArfafe(i).TIPO_OPERACION,tArfafe(i).EST_RES_CON,tArfafe(i).DETRACCION,tArfafe(i).PORC_DETRAC,
                       cGuiaTemp, cEnvAws, 'N'
                  );
             EXCEPTION
               WHEN OTHERS THEN
                  ROLLBACK;
                  DBMS_OUTPUT.PUT_LINE('>>> ERROR AL GUARDAR EN LA CABECERA DE LA FACTURA O BOLETA. '||SQLERRM);  
                  p_cError := '1,ERROR AL GUARDAR EN LA CABECERA DE LA FACTURA O BOLETA. '||SQLERRM;
             END;
           
         END LOOP;
       
       END IF;
    
    END IF;

    -- GUARDAR FACTU.ARFAFL
    IF p_cError = '0,OK' THEN
    
       FACTU.PR_FACTURA.SET_TDU_ARFAFL(p_cNoCia, p_cNoOrder, p_nNoDocu, cNoFactu, tArfafl, p_cError);

       BEGIN
         nCantArfafl := tArfafl.COUNT;
       EXCEPTION
         WHEN OTHERS THEN
            nCantArfafl := 0;
       END;
       DBMS_OUTPUT.PUT_LINE('>>> ENTRO PR_FACTU.SET_TDU_ARFAFL => CANTIDAD = '||nCantArfafl);
       IF nCantArfafl > 0 THEN
           FOR i IN 1..nCantArfafl LOOP
              DBMS_OUTPUT.PUT_LINE('----------- '||i);
             DBMS_OUTPUT.PUT_LINE('>>> tArfafl(i).NO_CIA = '||tArfafl(i).NO_CIA);
             DBMS_OUTPUT.PUT_LINE('>>> tArfafl(i).TIPO_DOC = '||tArfafl(i).TIPO_DOC);
             DBMS_OUTPUT.PUT_LINE('>>> tArfafl(i).NO_FACTU = '||tArfafl(i).NO_FACTU);
             DBMS_OUTPUT.PUT_LINE('>>> tArfafl(i).CONSECUTIVO = '||tArfafl(i).CONSECUTIVO);
             
              BEGIN
                  INSERT INTO FACTU.ARFAFL(
                       NO_CIA,TIPO_DOC,NO_FACTU,BODEGA,NO_ARTI,LOTE,CONSECUTIVO,CANTIDAD_FACT,
                       CANTIDAD_ENTR,CANTIDAD_DEV,TIPO_PRECIO,PRECIO_UNIT_ORIG,TOTAL,PRECIO_UNIT,
                       IND_PARENTESCO,DSCTO_CLIENTE,IGV,P_DSCTO3,M_DSCTO1,M_DSCTO2,M_DSCTO3,
                       IMP_IGV,IND_LIN,PRECIO_SIGV,TOTAL_LIN,TIPO_DOC_ALM,NO_DOCU_ALM,TIPO_BS,
                       OPER_EXONERADAS,OPER_GRATUITAS ,OPER_GRAVADAS,OPER_INAFECTAS,TIPO_AFECTACION,
                       PREC_IGV,MEDIDA,ICBPER)
                  VALUES (
                       tArfafl(i).NO_CIA, tArfafl(i).TIPO_DOC , tArfafl(i).NO_FACTU ,tArfafl(i).BODEGA,tArfafl(i).NO_ARTI,tArfafl(i).LOTE,tArfafl(i).CONSECUTIVO,tArfafl(i).CANTIDAD_FACT,
                       tArfafl(i).CANTIDAD_ENTR,tArfafl(i).CANTIDAD_DEV,tArfafl(i).TIPO_PRECIO,tArfafl(i).PRECIO_UNIT_ORIG,tArfafl(i).TOTAL,tArfafl(i).PRECIO_UNIT,
                       tArfafl(i).IND_PARENTESCO,tArfafl(i).DSCTO_CLIENTE,tArfafl(i).IGV,tArfafl(i).P_DSCTO3,tArfafl(i).M_DSCTO1,tArfafl(i).M_DSCTO2,tArfafl(i).M_DSCTO3,
                       tArfafl(i).IMP_IGV,tArfafl(i).IND_LIN,tArfafl(i).PRECIO_SIGV,tArfafl(i).TOTAL_LIN,tArfafl(i).TIPO_DOC_ALM,tArfafl(i).NO_DOCU_ALM,tArfafl(i).TIPO_BS,
                       tArfafl(i).OPER_EXONERADAS,tArfafl(i).OPER_GRATUITAS ,tArfafl(i).OPER_GRAVADAS,tArfafl(i).OPER_INAFECTAS,tArfafl(i).TIPO_AFECTACION,
                       tArfafl(i).PREC_IGV,tArfafl(i).MEDIDA,tArfafl(i).ICBPER
                       );
              EXCEPTION
               WHEN OTHERS THEN
                  DBMS_OUTPUT.PUT_LINE('>>> ERROR AL GUARDAR EL DETALLE DE LA FACTURA O BOLETA. '||SQLERRM);  
                  p_cError := '1,ERROR AL GUARDAR EL DETALLE DE LA FACTURA O BOLETA. '||SQLERRM;
              END;
           END LOOP;
       END IF;
       
    END IF;
    --DBMS_OUTPUT.PUT_LINE('>>> SE GUARDO LA FACTURA O BOLETA. '||p_cError);
    -- FACTURAR GUIA
    IF p_cError = '0,OK' THEN
       --DBMS_OUTPUT.PUT_LINE('>>> SE GUARDO LA FACTURA O BOLETA. ');
       FACTU.PR_GUIA.FACTURAR_GUIA_FICTA(p_cNoCia, cBodega, p_cNoGuia, cTipoDoc, cNoFactu, p_cError );
    END IF;
    
  END GUARDAR;
  
   /*---------------------------------------------------------------------------------------
   Nombre      : CREAR_ARCHIVO_SFS
   Proposito   : PROCEDIMIENTO QUE NOS PERMITE CRAR EL ARCHIVO PLANO PARA EL SFS - SUNAT
   Parametro  :

   Log de Cambios:
     Fecha        Autor                     Descripción
     05/10/2025   Robinzon Santana          Creador   
  -----------------------------------------------------------------------------------------*/
  PROCEDURE CREAR_ARCHIVO_SFS(
    P_NO_CIA IN FACTU.ARPFOE.NO_CIA%TYPE,
    P_TIPO_DOC IN FACTU.ARFAFE.TIPO_DOC%TYPE,  
    P_NO_FACTU IN FACTU.ARFAFE.NO_FACTU%TYPE
  ) IS
  
  Archivo UTL_FILE.FILE_TYPE;
  det_grat number :=1;
  p_venta varchar2(20);
  v_unit varchar2(20);
  v_venta number;
  tipo_precio varchar2(2):='01';
  V_SUMA_TRIBU NUMBER(12,2);
  -- DESCUENTO POR ITEM : 
  catalogo_desc_item varchar2(8);
  fact_desc_item varchar2(20);
  mont_desc_item varchar2(20);
  mont_base_desc_item varchar2(20);
  -- Variable para tipo de documento
  V_TIPO_DOC_CHAR VARCHAR2(1); -- F para Factura, B para Boleta
  
  -- Variables para datos de ARFAFE
  REC_ARFAFE FACTU.ARFAFE%ROWTYPE;

  kDirSfs14 CONSTANT VARCHAR2(10) := 'DIR_SFS_14';
  ---------------------------------------------------------------------
  CURSOR C_DET_ITEMS IS 
  SELECT  
          B.CONSECUTIVO A8,    -- NRO LINEA
          B.medida,
          B.CANTIDAD_FACT A1,  -- CANTIDAD
          B.TOTAL_LIN  A2,     -- PRECIO DE VENTA TOTAL LINEA CON IGV
          B.PRECIO_UNIT  A3,   -- PRECIO UNIT SIN IGV
          B.IGV A12,           -- % IGV
          ROUND(B.IMP_IGV,2)  A4, -- IGV
          NVL(B.ISC,0)   A20,  -- % ISC
          NVL(B.IMP_ISC,0) A17,-- ISC
          B.NO_ARTI A5,        -- CODIGO DE ARTICULO
          B.TIPO_BS A26,       -- TIPO DE BIEN O SERVICIO
          B.CONCEPTO A19,      -- CONCEPTO
          B.TIPO_AFECTACION A7,-- TIPO DE AFECCION
          ROUND(B.M_DSCTO3,2) A9,-- DESCUENTO SIN IGV
          B.P_DSCTO3 A10,      -- % DESCUENTO
          B.TOTAL  A11,        -- PRECIO TOTAL LINEA SIN IGV
          -- OPERACIONES CON TIPO AFECTACION
          B.OPER_GRAVADAS A13,  -- OPERACIONES GRAVADAS
          B.OPER_INAFECTAS A14, -- OPERACIONES INAFECTAS
          B.OPER_EXONERADAS A15,-- OPERACIONES EXONERADAS
          B.OPER_GRATUITAS A16, -- OPERACIONES GRATUITAS
          -- VALORES CALCULADOS
          ROUND( (B.TOTAL * (B.IGV/100)),2) A18, --Sumatoria de impuestos por línea
          (B.CANTIDAD_FACT*B.PRECIO_UNIT) A21,
          ROUND(PRECIO_UNIT_ORIG,2) A23, --precio original de la lista de precio
          ROUND(PRECIO_UNIT_ORIG*(B.P_DSCTO3/100),2) AS A24, --descuento x item
          (ROUND(PRECIO_UNIT_ORIG,5) - ROUND(PRECIO_UNIT_ORIG*(B.P_DSCTO3/100),5)) + 
          (ROUND(PRECIO_UNIT_ORIG,5) - ROUND(PRECIO_UNIT_ORIG*(B.P_DSCTO3/100),5))*ROUND((B.IGV/100),5) A25 --Precio de venta unitario(Incluye Desct y igv)
  FROM ARFAFE A,ARFAFL B
  WHERE   A.NO_CIA = B.NO_CIA AND
          A.TIPO_DOC = B.TIPO_DOC AND 
          A.NO_FACTU = B.NO_FACTU AND  
          A.NO_CIA   = P_NO_CIA AND
          A.TIPO_DOC = P_TIPO_DOC AND
          A.NO_FACTU = P_NO_FACTU
  ORDER BY B.CONSECUTIVO;
  ---------------------------------------------------------------------
  CURSOR MEDIO_PAGO IS 
  SELECT 
         NO_LINEA B1,
         TIPO_OPER B2, --TIPO OPERACION
         COD_OPER B3,  --CODIGO OPERACION
         IMP_TC B4,
         CLASE_TARJ B5,
         NO_OPERACION B6,
         FECHA_CH_DIFE B7,
         IMPORTE B8,
         VUELTO B9,
         TIPO_M B10
  FROM Artstrd_PVen
  WHERE NO_CIA = P_NO_CIA AND
        TIPO_DOC = P_TIPO_DOC AND
        NO_FACTU = P_NO_FACTU;
  ---------------------------------------------------------------------   
  -- VARIABLES EMPRESAS(ESCUELA O DELTA)
  LEYENDA VARCHAR2(4):=NULL;
  PAGINA_WEB VARCHAR2(200);
  EMAIL_EAA VARCHAR2(150);
  MONEDA VARCHAR2(8);
  PAIS_EAA VARCHAR2(8);
  COD_PAIS_SUNAT_EAA VARCHAR2(3);
  RQ VARCHAR2(500);
  T_DOC VARCHAR2(2);
  T_DOCUMENTO VARCHAR2(8);
  RUC_EAA VARCHAR2(11);
  TIPO_DOC_EMI VARCHAR2(5); 
  UBIGEO_EAA VARCHAR2(6);
  DIRECCION_EAA VARCHAR2(150);
  DEPA_EAA VARCHAR2(2);
  PROV_EAA VARCHAR2(2);
  DIST_EAA VARCHAR2(2);
  DEPA_EAA_DESC VARCHAR2(100);
  PROV_EAA_DESC VARCHAR2(100);
  DIST_EAA_DESC VARCHAR2(100);
  RAZON_SOCIAL_EAA VARCHAR2(100);
  TELEFONO_EAA VARCHAR2(100);
  
  -- SUCURSALES DE ESCUELA O DELTA
  UBIGEO_EAA_SUC VARCHAR2(6);
  DIRECCION_EAA_SUC VARCHAR2(150);
  DEPA_EAA_SUC VARCHAR2(2);
  PROV_EAA_SUC VARCHAR2(2);
  DIST_EAA_SUC VARCHAR2(2);
  DEPA_EAA_DESC_SUC VARCHAR2(100);
  PROV_EAA_DESC_SUC VARCHAR2(100);
  DIST_EAA_DESC_SUC VARCHAR2(100);
  URBANIZA_EAA_DESC_SUC VARCHAR2(100);
  TELEFONO_EAA_SUC VARCHAR2(12);
  COD_PV VARCHAR2(3);
  EMAIL_SUCURSAL VARCHAR2(150);

  --CLIENTE
  XNBR_CLIENTE VARCHAR2(300);
  UBIGEO_CLIENTE VARCHAR2(6);
  DIRECCION_CLIENTE VARCHAR2(150);
  EMAIL_CLIENTE VARCHAR2(150);
  PAIS_CLIENTE VARCHAR2(8);
  COD_PAIS_SUNAT_CLIENTE VARCHAR2(3);
  DEPA_CLIENTE VARCHAR2(2);
  PROV_CLIENTE VARCHAR2(2);
  DIST_CLIENTE VARCHAR2(2);
  DEPA_CLIENTE_DESC VARCHAR2(100);
  PROV_CLIENTE_DESC VARCHAR2(100);
  DIST_CLIENTE_DESC VARCHAR2(100);  
  XCOD_TIENDA  VARCHAR2(3); 
  NUM_DOC_CLI VARCHAR2(16);
  TIPO_DOC_CLI VARCHAR2(3);
  TELEFONO_CLIENTE VARCHAR2(12);

  --CLIENTE DIR ENTREGA
  XUBIGEO_CLIENTE VARCHAR2(6);
  XDIRECCION_CLIENTE VARCHAR2(150);
  XDEPA_CLIENTE VARCHAR2(2);
  XPROV_CLIENTE VARCHAR2(2);
  XDIST_CLIENTE VARCHAR2(2);
  XDEPA_CLIENTE_DESC VARCHAR2(100);
  XPROV_CLIENTE_DESC VARCHAR2(100);
  XDIST_CLIENTE_DESC VARCHAR2(100);
  
  --COMENTARIO --------------
  XCOMENTARIO1 VARCHAR2(100);
  XCOMENTARIO2 VARCHAR2(500);  
  ---------------------------
  F_VENCE DATE;
  HORA VARCHAR2(15);
  XDESC VARCHAR2(7);
  RUTA_RQ   VARCHAR2(80);
  v_cod_tributo    tablas_sunat_fe.codigo%type; 
  v_nom_tributo    tablas_sunat_fe.nombre_corto%type;
  v_cat_tributo    tablas_sunat_fe.categoria%type;
  v_valor_tributo  tablas_sunat_fe.valor%type;
  COD_GUIA   varchar2(2);
  COD_OTROS  varchar2(2);  
  DOC_RELACIONADO varchar2(300);  
  EMAIL_SUMIDERO varchar2(500); 
  NO_CLIENTE varchar2(11);
  UNI_MED_SUNAT varchar2(5);
  DES_ARTICULO varchar2(100);
  V_FP VARCHAR2(5); -- Solo para facturas
BEGIN
  
  -- Obtener datos principales de ARFAFE
  BEGIN
    SELECT *
    INTO REC_ARFAFE
    FROM FACTU.ARFAFE
    WHERE NO_CIA = P_NO_CIA 
      AND TIPO_DOC = P_TIPO_DOC 
      AND NO_FACTU = P_NO_FACTU;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      -- ERROR('No se encontró el documento: CIA='||P_NO_CIA||' TIPO='||P_TIPO_DOC||' NUM='||P_NO_FACTU);
      RAISE_APPLICATION_ERROR(-20001, 'No se encontró el documento: CIA='||P_NO_CIA||' TIPO='||P_TIPO_DOC||' NUM='||P_NO_FACTU);
    WHEN OTHERS THEN
      RAISE_APPLICATION_ERROR(-20001,'Error al consultar ARFAFE: '||SQLERRM);
  END;
  
  -- Determinar tipo de documento
  V_TIPO_DOC_CHAR := UPPER(REC_ARFAFE.TIPO_DOC);
  
 --===========================================================================================================================================
  --                                                  DATOS GENERALES DE EMPRESAS ESCUELA O DELTA
  --===========================================================================================================================================
  -- PAGINA WEB, TELEFONO,RAZON SOCIAL Y RUC
  DBMS_OUTPUT.Put_Line('RAZON SOCIAL Y RUC');
  BEGIN
    SELECT PAGINA_WEB , TELEFONOS ,NOMBRE,NO_CLIENTE_ONLINE
    INTO PAGINA_WEB , TELEFONO_EAA,RAZON_SOCIAL_EAA,RUC_EAA
    FROM ARFAMC 
    WHERE NO_CIA = P_NO_CIA;
  EXCEPTION WHEN NO_DATA_FOUND THEN
     RAISE_APPLICATION_ERROR(-20001,'EL NO_CIA '||P_NO_CIA||' , NO ENCONTRADO. ');
  END;

  -- Asignar página web por defecto según tipo de documento
  IF PAGINA_WEB is null then
    IF V_TIPO_DOC_CHAR = 'F' THEN
      PAGINA_WEB :='www.hs-import.com';
    ELSE
      PAGINA_WEB :='www.ABMIN.com';
    END IF;
  END IF;
  
  --======= CORREO y/o EMAIL SUMIDERO DE EMPRESA ESCUELA / DELTA =========
  BEGIN
      SELECT NOMBRE
      INTO  EMAIL_SUMIDERO
      FROM ATRIBUTOS
      WHERE CLASE = 85 AND
            MODULO ='BAS' AND
            CODIGO = P_NO_CIA;
  EXCEPTION WHEN NO_DATA_FOUND THEN
     null;
  END;
   DBMS_OUTPUT.put_line('FIN CORREO y/o EMAIL SUMIDERO DE EMPRESA ESCUELA / DELTA');
  --=====================================================================  
  IF TELEFONO_EAA is null then
    TELEFONO_EAA :='';
  END IF;
  
  BEGIN
    SELECT TIPO_DOCUMENTO,COD_PAIS,EMAIL
    INTO TIPO_DOC_EMI,PAIS_EAA,EMAIL_EAA
    FROM ARCCMC
    WHERE NO_CIA=P_NO_CIA AND
          NO_CLIENTE=RUC_EAA;
  EXCEPTION WHEN NO_DATA_FOUND THEN
     TIPO_DOC_EMI :='';
     PAIS_EAA :='';
     EMAIL_EAA :=EMAIL_SUMIDERO;  
  END;  
  /*
  DBMS_OUTPUT.put_line('FIN ARCCMC');
  
  BEGIN
      SELECT CODIGO
      INTO COD_PAIS_SUNAT_EAA
      FROM CONTA.TABLAS_SUNAT_FE 
      WHERE CLASE = 4
      AND VALOR = PAIS_EAA;
  EXCEPTION WHEN NO_DATA_FOUND THEN
    RAISE_APPLICATION_ERROR(-20001,'no existe codigo de pais sunat...'||' pais : '||PAIS_EAA);
  END;
  
  DBMS_OUTPUT.put_line('VA ENTRAR UBIGEO DE ESCUELA O DELTA ');
  */
  --UBIGEO DE ESCUELA O DELTA
  BEGIN
  SELECT CODI_DEPA||CODI_PROV||CODI_DIST,DIRECCION,CODI_DEPA,CODI_PROV,CODI_DIST
  INTO UBIGEO_EAA,DIRECCION_EAA,DEPA_EAA,PROV_EAA,DIST_EAA
  FROM ARCCTDA 
  WHERE NO_CIA =P_NO_CIA AND
        NO_CLIENTE = RUC_EAA AND 
        COD_TIENDA = REC_ARFAFE.COD_TIENDA;
  EXCEPTION WHEN NO_DATA_FOUND THEN
    UBIGEO_EAA :='';
    DIRECCION_EAA :='';
    DEPA_EAA := '';
    PROV_EAA :='';
    DIST_EAA :='';    
  END;
  DBMS_OUTPUT.put_line('FIN ENTRAR UBIGEO DE ESCUELA O DELTA ');
  --DEPARTAMENTO DE ESCUELA O DELTA 
  BEGIN
  SELECT DESC_DEPA
  INTO DEPA_EAA_DESC
  FROM ARCCDP 
  WHERE NO_CIA =P_NO_CIA AND
        CODI_DEPA = DEPA_EAA;
  EXCEPTION WHEN NO_DATA_FOUND THEN
    DEPA_EAA_DESC :='';
  END;
   DBMS_OUTPUT.put_line('FIN DEPARTAMENTO DE ESCUELA O DELTA ');
  --PROVINCIA DE ESCUELA O DELTA 
  BEGIN
  SELECT DESC_PROV
  INTO PROV_EAA_DESC
  FROM ARCCPR 
  WHERE NO_CIA =P_NO_CIA AND
        CODI_DEPA = DEPA_EAA AND 
        CODI_PROV = PROV_EAA;
  EXCEPTION WHEN NO_DATA_FOUND THEN
    PROV_EAA_DESC :='';
  END;
  
  --DISTRITO DE ESCUELA O DELTA 
  BEGIN
  SELECT DESC_DIST
  INTO DIST_EAA_DESC
  FROM ARCCDI 
  WHERE NO_CIA =P_NO_CIA AND
        CODI_DEPA = DEPA_EAA AND 
        CODI_PROV = PROV_EAA AND 
        CODI_DIST = DIST_EAA;
  EXCEPTION WHEN NO_DATA_FOUND THEN
    DIST_EAA_DESC :='';
  END;
  DBMS_OUTPUT.put_line('VA ENTRAR A UBIGEO DE ESCUELA O DELTA SUCURSAL');
  -------------------------------------------- UBIGEO DE ESCUELA O DELTA SUCURSAL --------------------------------------------------- 
  IF REC_ARFAFE.CENTRO IN('32','33','34') THEN
    COD_PV := '000';
  ELSIF REC_ARFAFE.CENTRO IN('35') THEN
    COD_PV := REC_ARFAFE.CENTRO; 
  END IF;
  --
  BEGIN
    SELECT CODI_DEPA||CODI_PROV||CODI_DIST,DIRECCION,CODI_DEPA,CODI_PROV,CODI_DIST,TELEF1,CORREOELECTRO,URBANIZA
    INTO UBIGEO_EAA_SUC,DIRECCION_EAA_SUC,DEPA_EAA_SUC,PROV_EAA_SUC,DIST_EAA_SUC,TELEFONO_EAA_SUC,EMAIL_SUCURSAL,URBANIZA_EAA_DESC_SUC
    FROM SUCURSAL_PTOVTA 
    WHERE NO_CIA = P_NO_CIA AND 
          COD_SUCURSAL = REC_ARFAFE.COD_TIENDA AND 
          COD_PTO_VTA = COD_PV;
  EXCEPTION WHEN NO_DATA_FOUND THEN
    UBIGEO_EAA_SUC :='';
    DIRECCION_EAA_SUC :='';
    DEPA_EAA_SUC := '';
    PROV_EAA_SUC :='';
    DIST_EAA_SUC :='';
    TELEFONO_EAA_SUC :='';
    EMAIL_SUCURSAL :='';
    URBANIZA_EAA_DESC_SUC:='';
  END;

  --DEPARTAMENTO SUCURSAL
  BEGIN
    SELECT DESC_DEPA
    INTO DEPA_EAA_DESC_SUC
    FROM ARCCDP 
    WHERE NO_CIA = P_NO_CIA AND
          CODI_DEPA = DEPA_EAA_SUC;
  EXCEPTION WHEN NO_DATA_FOUND THEN
    DEPA_EAA_DESC_SUC :='';
  END;

  --PROVINCIA SUCURSAL
  BEGIN
    SELECT DESC_PROV
    INTO PROV_EAA_DESC_SUC
    FROM ARCCPR 
    WHERE NO_CIA = P_NO_CIA AND
          CODI_DEPA = DEPA_EAA_SUC AND 
          CODI_PROV = PROV_EAA_SUC;
  EXCEPTION WHEN NO_DATA_FOUND THEN
    PROV_EAA_DESC_SUC :='';
  END;

  --DISTRITO SUCURSAL
  BEGIN
  SELECT DESC_DIST
  INTO DIST_EAA_DESC_SUC
  FROM ARCCDI 
  WHERE NO_CIA = P_NO_CIA AND
        CODI_DEPA = DEPA_EAA_SUC AND 
        CODI_PROV = PROV_EAA_SUC AND 
        CODI_DIST = DIST_EAA_SUC;
  EXCEPTION WHEN NO_DATA_FOUND THEN
    DIST_EAA_DESC_SUC :='';
  END;  
  
  -- Limpiar caracteres especiales
  BEGIN
    RAZON_SOCIAL_EAA := translate(RAZON_SOCIAL_EAA,'T|%•$¿?^Çªº°~€¬ç¡','T');
    DIRECCION_EAA := translate(DIRECCION_EAA,'T|%•$¿?^Çªº°~€¬ç¡','T');
    DIRECCION_EAA_SUC := translate(DIRECCION_EAA_SUC,'T|%•$¿?^Çªº°~€¬ç¡','T');
    DIST_EAA_DESC := translate(DIST_EAA_DESC,'T|%•$¿?^Çªº°~€¬ç¡','T');
    DIST_EAA_DESC_SUC := translate(DIST_EAA_DESC_SUC,'T|%•$¿?^Çªº°~€¬ç¡','T');
  END;
  
  --===========================================================================================================================================
  --                                                 DATOS GENERALES DE CLIENTES
  --===========================================================================================================================================
  BEGIN
    SELECT EMAIL, COD_PAIS,TIPO_DOCUMENTO,NU_DOCUMENTO
    INTO EMAIL_CLIENTE, PAIS_CLIENTE,TIPO_DOC_CLI,NUM_DOC_CLI
    FROM ARCCMC 
    WHERE no_cia =P_NO_CIA and
          NO_CLIENTE = REC_ARFAFE.NO_CLIENTE; 
  EXCEPTION WHEN NO_DATA_FOUND  THEN
    EMAIL_CLIENTE :=EMAIL_SUMIDERO; 
    PAIS_CLIENTE := '';
    TIPO_DOC_CLI := 'OTR';
  END;
  
  --=======================================================================
  IF LENGTH(REC_ARFAFE.NO_CLIENTE) = 11 AND REC_ARFAFE.NO_CLIENTE NOT LIKE ('9%') THEN
     TIPO_DOC_CLI :='RUC';
  END IF;
  --=======================================================================
  IF EMAIL_CLIENTE IS NULL OR EMAIL_CLIENTE = '' THEN
    EMAIL_CLIENTE :=EMAIL_SUMIDERO; 
  END IF;
  --=======================================================================
  BEGIN
      SELECT CODIGO
      INTO COD_PAIS_SUNAT_CLIENTE
      FROM CONTA.TABLAS_SUNAT_FE 
      WHERE CLASE = 4
      AND VALOR = PAIS_CLIENTE;
  EXCEPTION WHEN NO_DATA_FOUND THEN
    RAISE_APPLICATION_ERROR(-20001,'no existe codigo de pais sunat...'||' pais cli : '||PAIS_CLIENTE);
  END;  
  
  -- TIPO DOCUMETO DE CLIENTE (DNI, RUC U OTROS)
  BEGIN
  SELECT CODIGO
  INTO T_DOCUMENTO
  FROM TABLAS_SUNAT_FE
  WHERE CLASE = '6'
  AND VALOR = TIPO_DOC_CLI;
  EXCEPTION WHEN NO_DATA_FOUND THEN
    T_DOCUMENTO :='0';
  END;

  ------------------------------------ UBIGEO DEL CLIENTE ------------------------------------
  BEGIN
  SELECT CODI_DEPA||CODI_PROV||CODI_DIST,DIRECCION,CODI_DEPA,CODI_PROV,CODI_DIST,TELEFONO
  INTO UBIGEO_CLIENTE,DIRECCION_CLIENTE,DEPA_CLIENTE,PROV_CLIENTE,DIST_CLIENTE,TELEFONO_CLIENTE
  FROM ARCCTDA 
  WHERE NO_CIA =P_NO_CIA AND
        NO_CLIENTE = REC_ARFAFE.NO_CLIENTE AND 
        COD_TIENDA = REC_ARFAFE.COD_TIENDA;
  EXCEPTION WHEN NO_DATA_FOUND THEN
    UBIGEO_CLIENTE :='';
    DIRECCION_CLIENTE :='';
    DEPA_CLIENTE :='';
    PROV_CLIENTE :='';
    DIST_CLIENTE :='';
    TELEFONO_CLIENTE :='';
  END;
  -------------------- DEPARTAMENTO DEL CLIENTE ------------------------
  BEGIN
  SELECT DESC_DEPA
  INTO DEPA_CLIENTE_DESC
  FROM ARCCDP 
  WHERE NO_CIA =P_NO_CIA AND
        CODI_DEPA = DEPA_CLIENTE;
  EXCEPTION WHEN NO_DATA_FOUND THEN
    DEPA_CLIENTE_DESC :='';
  END;
  -------------------- PROVINCIA DEL CLIENTE ---------------------------
  BEGIN
  SELECT DESC_PROV
  INTO PROV_CLIENTE_DESC
  FROM ARCCPR 
  WHERE NO_CIA =P_NO_CIA AND
        CODI_DEPA = DEPA_CLIENTE AND 
        CODI_PROV = PROV_CLIENTE;
  EXCEPTION WHEN NO_DATA_FOUND THEN
    PROV_CLIENTE_DESC :='';
  END;
  -------------------- DISTRITO DEL CLIENTE ---------------------------
  BEGIN
  SELECT DESC_DIST
  INTO DIST_CLIENTE_DESC
  FROM ARCCDI 
  WHERE NO_CIA =P_NO_CIA AND
        CODI_DEPA = DEPA_CLIENTE AND 
        CODI_PROV = PROV_CLIENTE AND 
        CODI_DIST = DIST_CLIENTE;
  EXCEPTION WHEN NO_DATA_FOUND THEN
    DIST_CLIENTE_DESC :='';
  END;
  
  BEGIN
  SELECT COD_TIENDA
  INTO XCOD_TIENDA
  FROM ARCCTDA
  WHERE NO_CIA=P_NO_CIA AND
        NO_CLIENTE = REC_ARFAFE.NO_CLIENTE AND 
        COD_TIENDA = REC_ARFAFE.COD_DIR_ENTREGA; 
  EXCEPTION WHEN NO_DATA_FOUND THEN
    XCOD_TIENDA :='';
  END;    
  
  ---- ENCONTRAR EL CORREO DEL CLIENTE 
 IF SUBSTR(REC_ARFAFE.NO_CLIENTE,1,1) <> '9' THEN
  BEGIN
    SELECT EMAIL 
    INTO EMAIL_CLIENTE
    FROM ARCCMC 
    WHERE NO_CIA = P_NO_CIA AND
          NO_CLIENTE = REC_ARFAFE.NO_CLIENTE; 
    EXCEPTION WHEN NO_DATA_FOUND  THEN
        EMAIL_CLIENTE :=EMAIL_SUMIDERO;  
  END;
 ELSE
  BEGIN
    SELECT EMAIL_PEDIDO 
    INTO EMAIL_CLIENTE
    FROM ARPFOE 
    WHERE NO_CIA = P_NO_CIA AND
          NO_ORDEN = REC_ARFAFE.NO_ORDEN; 
    EXCEPTION WHEN NO_DATA_FOUND  THEN
        EMAIL_CLIENTE :=EMAIL_SUMIDERO;  
  END;
 END IF;
   
  IF EMAIL_CLIENTE IS NULL OR EMAIL_CLIENTE = '' THEN
     EMAIL_CLIENTE :=EMAIL_SUMIDERO;   
  END IF;  
  
  --NBR_CLIENTE
  IF SUBSTR(REC_ARFAFE.NO_CLIENTE,1,1) NOT IN ('0','1','2','3','4','5','6','7','8') THEN 
    XNBR_CLIENTE := REC_ARFAFE.NOMBRE_DIGI;
  ELSE
    XNBR_CLIENTE := REC_ARFAFE.NBR_CLIENTE;
  END IF;
  
  -- Limpiar caracteres especiales del nombre del cliente
  BEGIN
    XNBR_CLIENTE := translate(XNBR_CLIENTE,'T|%•$¿?^Çªº°~€¬ç¡','T');
    DIRECCION_CLIENTE := translate(DIRECCION_CLIENTE,'T|%•$¿?^Çªº°~€¬ç¡','T');
    DIST_CLIENTE_DESC := translate(DIST_CLIENTE_DESC,'T|%•$¿?^Çªº°~€¬ç¡','T');
  END; 
  
  IF REC_ARFAFE.COD_T_PED in ('1320','1321','1350', '1351') THEN
    T_DOCUMENTO :='1';
  END IF;
  
  DBMS_OUTPUT.put_line('VA ENTRAR A DATOS DEL DOCUMENTO GENERADO');
  
  --===========================================================================================================================================
  --                                                 DATOS DEL DOCUMENTO GENERADO
  --===========================================================================================================================================
  -- TIPO DE DOCUMENTO (FACTURA (01), BOLETA(03) U OTROS)
  BEGIN
  SELECT CODIGO
  INTO T_DOC
  FROM CONTA.TABLAS_SUNAT_FE 
  WHERE CLASE = '01'
  AND VALOR = REC_ARFAFE.TIPO_DOC;
  EXCEPTION WHEN NO_DATA_FOUND THEN
    T_DOC :='';
  END;  
  
  -- MONEDA
  BEGIN
    SELECT CODIGO
    INTO MONEDA
    FROM TABLAS_SUNAT_FE
    WHERE CLASE = '2'
    AND VALOR = REC_ARFAFE.MONEDA;  
  EXCEPTION WHEN NO_DATA_FOUND THEN
    MONEDA :='';
  END;

  -- DATOS DEL DOCUMENTO (diferente query para F y B)
  BEGIN  
   SELECT FECHA_VENCE,TO_CHAR(FEC_CREA,'HH24:MI:SS')
   INTO   F_VENCE,HORA
   FROM ARFAFE
   WHERE NO_CIA=P_NO_CIA AND
         TIPO_DOC=V_TIPO_DOC_CHAR AND
         NO_FACTU=P_NO_FACTU;
  EXCEPTION WHEN NO_DATA_FOUND THEN
    F_VENCE := NULL;
    HORA := NULL;
  END;
  
  --===========================================================================================================================================
  --                                                 DATOS DE LA ENTREGA
  --===========================================================================================================================================
  -- DATOS DE LA ENTREGA ------------------------------------
  BEGIN
  SELECT CODI_DEPA||CODI_PROV||CODI_DIST,DIRECCION,CODI_DEPA,CODI_PROV,CODI_DIST
  INTO XUBIGEO_CLIENTE,XDIRECCION_CLIENTE,XDEPA_CLIENTE,XPROV_CLIENTE,XDIST_CLIENTE
  FROM ARCCTDA 
  WHERE NO_CIA =P_NO_CIA AND
        NO_CLIENTE = REC_ARFAFE.NO_CLIENTE AND 
        COD_TIENDA = REC_ARFAFE.COD_DIR_ENTREGA;
  EXCEPTION WHEN NO_DATA_FOUND THEN
    XUBIGEO_CLIENTE :='';
    XDIRECCION_CLIENTE :='';
    XDEPA_CLIENTE :='';
    XPROV_CLIENTE :='';
    XDIST_CLIENTE :='';
  END;
  ---------- DEPARTAMENTO DE ENTREGA -------------------------------
  BEGIN
  SELECT DESC_DEPA
  INTO XDEPA_CLIENTE_DESC
  FROM ARCCDP 
  WHERE NO_CIA =P_NO_CIA AND
        CODI_DEPA = XDEPA_CLIENTE;
  EXCEPTION WHEN NO_DATA_FOUND THEN
    XDEPA_CLIENTE_DESC :='';
  END;
  ---------- PROVINCIA DE ENTREGA -------------------------------
  BEGIN
  SELECT DESC_PROV
  INTO XPROV_CLIENTE_DESC
  FROM ARCCPR 
  WHERE NO_CIA =P_NO_CIA AND
        CODI_DEPA = XDEPA_CLIENTE AND 
        CODI_PROV = XPROV_CLIENTE;
  EXCEPTION WHEN NO_DATA_FOUND THEN
    XPROV_CLIENTE_DESC :='';
  END;
  ---------- DISTRITO DE ENTREGA -------------------------------
  BEGIN
  SELECT DESC_DIST
  INTO XDIST_CLIENTE_DESC
  FROM ARCCDI 
  WHERE NO_CIA =P_NO_CIA AND
        CODI_DEPA = XDEPA_CLIENTE AND 
        CODI_PROV = XPROV_CLIENTE AND 
        CODI_DIST = XDIST_CLIENTE;
  EXCEPTION WHEN NO_DATA_FOUND THEN
    XDIST_CLIENTE_DESC :='';
  END;  
  --==================================================================================================================================
  -- RQ Y DATOS DE ESCUELA O DELTA -------------------------------------------------------------  
  /*
  BEGIN
    SELECT RUC||'-'||T_DOC||'-'||substr(REC_ARFAFE.no_factu,1,4)||'-'||SUBSTR(P_NO_FACTU,LENGTH(substr(REC_ARFAFE.no_factu,1,4))+1,LENGTH(P_NO_FACTU)- LENGTH(substr(REC_ARFAFE.no_factu,1,4))+1 ),RUC 
    INTO RQ,RUC_EAA
    FROM ARCGMC
    WHERE NO_CIA=P_NO_CIA;
  EXCEPTION WHEN NO_DATA_FOUND THEN
    RQ :='';
    RUC_EAA :='';
  END;
  */
  BEGIN
    SELECT RUC_EAA||'-'||T_DOC||'-'||substr(REC_ARFAFE.no_factu,1,4)||'-'||SUBSTR(P_NO_FACTU,LENGTH(substr(REC_ARFAFE.no_factu,1,4))+1,LENGTH(P_NO_FACTU)- LENGTH(substr(REC_ARFAFE.no_factu,1,4))+1 ),RUC_EAA 
    INTO RQ,RUC_EAA
    FROM DUAL;
  EXCEPTION WHEN NO_DATA_FOUND THEN
    RQ :='';
    RUC_EAA :='';
  END; 
  
  -- DIRECTORIO DE ARCHIVOS PLANOS : RUTA DONDE SE GUARDARAN LOS TXT DE FACTU ELECTRONICA -----
  /*BEGIN
     SELECT NOMBRE 
     into RUTA_RQ   
     FROM ATRIBUTOS
     WHERE CLASE ='83' AND
           MODULO ='BAS' AND
           VALOR = 2 AND
           CODIGO <>'99999999';
  EXCEPTION WHEN NO_DATA_FOUND THEN
    RAISE_APPLICATION_ERROR(-20001,'No tiene registrado la ruta');
  END;
  */ 
  ---------------------------------------------------------------------------
  BEGIN
  SELECT NOMBRE
  INTO RAZON_SOCIAL_EAA
  FROM ARCCMC 
  WHERE NO_CIA=P_NO_CIA AND 
        NO_CLIENTE = RUC_EAA;
  EXCEPTION WHEN NO_DATA_FOUND THEN
    RAZON_SOCIAL_EAA :='';
  END;  
  --==================================================================================================================================
  -- USO DEL CURSOR
  FOR i IN C_DET_ITEMS LOOP            
    IF i.A7 IN ('11','12','13','14','15','16','21','31','32','33','34','35','36') THEN
      det_grat := 2;
      tipo_precio := '02';
      LEYENDA :='1002';
    END IF;
  END LOOP;
  ------------------------------------------------------------------------------------------------
  IF V_TIPO_DOC_CHAR = 'F' AND NVL(LEYENDA,'0') <> '1002' THEN
    XCOMENTARIO1 :='';
    XCOMENTARIO2 :='';      
  ELSE
    XCOMENTARIO1 :='';
    XCOMENTARIO2 :='';
  END IF;
  ------------------------------------------------------------------------------------------------
  IF REC_ARFAFE.NO_GUIA IS NOT NULL THEN
    COD_GUIA := '09';
  END IF;
  
  IF REC_ARFAFE.NO_ORDEN IS NOT NULL THEN
    DOC_RELACIONADO :=REC_ARFAFE.NO_ORDEN;
    COD_OTROS := '99';
  END IF;
  
  IF REC_ARFAFE.CODI_COLE IS NOT NULL THEN
    COD_OTROS := '99';
  END IF;  
  --==================================================================================================================================
  --                                     CREACION DE LOS ARCHIVOS .TXT PARA FACTURACION ELECTRONICA
  --==================================================================================================================================
    -- CREACION DEL ARCHIVO CABECERA 
    Archivo := UTL_FILE.FOPEN(kDirSfs14, RQ||'.CAB', 'W', 32767);
    
    NO_CLIENTE := REC_ARFAFE.NO_CLIENTE;
   ----------------------------------------- DATOS DEL RECEPTOR ---------------------------------------------------------------------------------------------------------------------------------- 
    -- Lógica específica para boletas con DNI
    IF V_TIPO_DOC_CHAR = 'B' AND REC_ARFAFE.TIPO_DOC_CLI IN('DNI') THEN
      T_DOCUMENTO := '1';
      NO_CLIENTE := REC_ARFAFE.NUM_DOC_CLI;
    END IF;
    
    -- CABECERA DEL ARCHIVO PLANO
    UTL_FILE.PUT_LINE(Archivo,RTRIM(LTRIM(REC_ARFAFE.TIPO_OPERACION))||'|'|| --  #1   TIPO DE OPERACION
                TO_CHAR(REC_ARFAFE.FECHA,'YYYY-MM-DD')||'|'||-- #2 FECHA DE EMISIÓN
                TO_CHAR(SYSDATE,'HH24:MI:SS')||'|'||--  #3  HORA DE EMISIÓN
                -- Fecha de vencimiento: para facturas usar F_VENCE, para boletas usar fecha de emisión
                CASE WHEN V_TIPO_DOC_CHAR = 'F' THEN TO_CHAR(REC_ARFAFE.FECHA_VENCE,'YYYY-MM-DD')
                     ELSE TO_CHAR(REC_ARFAFE.FECHA,'YYYY-MM-DD') END ||'|'||-- #4 FECHA DE VENCIMIENTO
                '0000'||'|'|| -- #5 Código del domicilio fiscal o de local anexo del emisor
                T_DOCUMENTO||'|'|| --#6 TIPO DE DOCUMENTO DE IDENTIDAD DEL ADQUIERIENTE O USUARIO
                SUBSTR(NO_CLIENTE,1,15)||'|'|| --#7 NUMERO DE DOCUMENTO DE IDENTIDAD DEL ADQUIRIENTE O USUARIO                
                SUBSTR(NVL(XNBR_CLIENTE,'ROBIN'),1,1500)||'|'|| --#8 APELIIDOS Y NOMBRES, DENOMINACION O RAZON SOCIAL
                MONEDA||'|'|| --#9 MONEDA               
                CONV_NUM_CAR(ROUND(REC_ARFAFE.IMPUESTO,2),2)||'|'|| --#10 SUMATORIA TRIBUTOS
                CONV_NUM_CAR(ROUND(REC_ARFAFE.VALOR_VENTA,2),2)||'|'|| --#11 Total valor de venta
                CONV_NUM_CAR(ROUND(REC_ARFAFE.TOTAL,2),2)||'|'|| --#12 Total PRECIO DE VENTA
                CONV_NUM_CAR(ROUND(REC_ARFAFE.T_DESCUENTO,2),2)||'|'|| --#13 TOTAL DESCUENTO
                CONV_NUM_CAR(ROUND(0,2),2)||'|'|| --#14 (Monto total de otros cargos del comprobante)
                CONV_NUM_CAR(ROUND(0,2),2)||'|'|| --#15 (Monto total de anticipos del comprobante)
                CONV_NUM_CAR(ROUND(REC_ARFAFE.TOTAL,2),2)||'|'|| --#16 IMPORTE TOTAL DE LA VENTA              
                '2.1'||'|2.0'); --#17 y 18              
    UTL_FILE.FCLOSE(Archivo);
    
    --##### OPERACIONES EXONERADAS ##################################################################################### 
    IF REC_ARFAFE.OPER_EXONERADAS > 0 THEN  
       Archivo := UTL_FILE.FOPEN(kDirSfs14, RQ||'.TRI', 'W', 32767);
       UTL_FILE.PUT_LINE(Archivo,'9997'||'|'|| --  #1   Código de unidad de medida por ítem
                'EXO'||'|'||-- #2 Nombre de tributo
                'VAT'||'|'||--  #3  Código de tipo de tributo
                 CONV_NUM_CAR(ROUND(REC_ARFAFE.VALOR_VENTA,2),2)||'|'|| --#4 Base imponible
                CONV_NUM_CAR(ROUND(REC_ARFAFE.IMPUESTO,2),2) --#5 Monto de Tirbuto por ítem
                );  
         UTL_FILE.FCLOSE(Archivo);
    END IF;
    --##### OPERACIONES INAFECTAS #####################################################################################      
    IF REC_ARFAFE.OPER_INAFECTAS > 0 THEN   
       Archivo := UTL_FILE.FOPEN(kDirSfs14, RQ||'.TRI', 'W', 32767);
       UTL_FILE.PUT_LINE(Archivo,'9998'||'|'|| --  #1   Código de unidad de medida por ítem
                'INA'||'|'||-- #2 Nombre de tributo
                'FRE'||'|'||--  #3  Código de tipo de tributo
                CONV_NUM_CAR(ROUND(REC_ARFAFE.VALOR_VENTA,2),2)||'|'|| --#4 Base imponible
                CONV_NUM_CAR(ROUND(REC_ARFAFE.IMPUESTO,2),2) --#5 Monto de Tirbuto por ítem
                ); 
       UTL_FILE.FCLOSE(Archivo);
    END IF; 
    --##### OPERACIONES GRATUITAS #####################################################################################
    IF REC_ARFAFE.OPER_GRATUITAS > 0 THEN 
       Archivo := UTL_FILE.FOPEN(kDirSfs14, RQ||'.TRI', 'W', 32767);
       UTL_FILE.PUT_LINE(Archivo,'9996'||'|'|| --  #1   Código de unidad de medida por ítem
                'GRA'||'|'||-- #2 Nombre de tributo
                'FRE'||'|'||--  #3  Código de tipo de tributo
                CONV_NUM_CAR(ROUND(REC_ARFAFE.VALOR_VENTA,2),2)||'|'|| --#4 Base imponible
                CONV_NUM_CAR(ROUND(REC_ARFAFE.IMPUESTO,2),2) --#5 Monto de Tirbuto por ítem
                );  
       UTL_FILE.FCLOSE(Archivo);   
    END IF;     
    --##### OPERACIONES GRAVADAS #####################################################################################          
    IF REC_ARFAFE.OPER_GRAVADAS > 0 THEN
       Archivo := UTL_FILE.FOPEN(kDirSfs14, RQ||'.TRI', 'W', 32767);
       UTL_FILE.PUT_LINE(Archivo,'1000'||'|'|| --  #1   Código de unidad de medida por ítem
                'IGV'||'|'||-- #2 Nombre de tributo
                'VAT'||'|'||--  #3  Código de tipo de tributo
                CONV_NUM_CAR(ROUND(REC_ARFAFE.VALOR_VENTA,2),2)||'|'|| --#4 Base imponible
                CONV_NUM_CAR(ROUND(REC_ARFAFE.IMPUESTO,2),2) --#5 Monto de Tirbuto por ítem
                );            
       UTL_FILE.FCLOSE(Archivo);
    END IF;
  
    -- CREACION DEL ARCHIVO DETALLE 
    Archivo := UTL_FILE.FOPEN(kDirSfs14, RQ||'.DET', 'W', 32767);
    -----------------------------------------  DATALLES ------------------------------------------------------------
    FOR i IN C_DET_ITEMS LOOP
      IF i.A7 = '10' OR i.A7 = '12' THEN 
         V_SUMA_TRIBU := i.A4;
      ELSIF i.A7 = '20' THEN
        V_SUMA_TRIBU := i.A15; 
      ELSIF i.A7 = '30' THEN
        V_SUMA_TRIBU := i.A14;
      ELSIF i.A7 ='21' THEN 
         V_SUMA_TRIBU := i.A16;
      ELSE
         V_SUMA_TRIBU := NULL;
      END IF; 
      -- Codigo, nombre y categoria de tributo. catalogo 5 --------
      BEGIN
         SELECT CODIGO, NOMBRE_CORTO, CATEGORIA, VALOR
         INTO v_cod_tributo,v_nom_tributo,v_cat_tributo,v_valor_tributo
         FROM CONTA.TABLAS_SUNAT_FE
         WHERE CODIGO = COD_TIPO_TRIBUTO(i.A7) AND 
               CLASE ='5';
      EXCEPTION WHEN NO_DATA_FOUND THEN 
          null;
      END;
      -- PARA ARTICULOS DE REGALO --------------------------------- 
      IF v_cod_tributo IN ('9996') THEN 
        v_venta :=0;  
        p_venta := LTRIM(TO_CHAR(ROUND(i.A11,5),'9990.99900'));
        v_unit := LTRIM(TO_CHAR(ROUND(0,5),'9990.99900'));
      ELSE
        v_venta := ROUND(i.A11,2);
        p_venta := LTRIM(TO_CHAR(ROUND(i.A25,5),'9990.99900'));
        v_unit := LTRIM(TO_CHAR(ROUND(i.A3,5),'9990.99900'));
      END IF;
      
      IF i.A26 = 'B' THEN
        UNI_MED_SUNAT := FACTU.UNI_MED_SUNAT(P_NO_CIA,i.A5);
        DES_ARTICULO := DESCRIP_ARTI(P_NO_CIA,i.A5);
      ELSE
        begin
          SELECT LTRIM(DESCRIPCION)
          INTO DES_ARTICULO
          FROM ARPFOL
          WHERE NO_CIA = P_NO_CIA AND
                NO_ARTI = i.A5 AND
                NO_ORDEN = REC_ARFAFE.NO_ORDEN;
          EXCEPTION WHEN NO_DATA_FOUND THEN null;
        end;
        UNI_MED_SUNAT := nvl(i.medida,'NIU');
      END IF;
            
      -- FACTURADOR SUNAT (DETALLE)  =============================================================================================
       UTL_FILE.PUT_LINE(Archivo,UNI_MED_SUNAT||'|'|| -- 1 (Unidad de medida por ítem)
                      CONV_NUM_CAR(ROUND(i.A1,2),2)||'|'|| -- 2 (Cantidad de item.)
                      i.A5||'|'|| -- 3 (Código de producto del ítem )
                      '-'||'|'|| --  4 (Código de producto de SUNAT )
                      RTRIM((LTRIM(DES_ARTICULO)))||'|'|| -- 5 (Descripción detallada del servicio prestado, bien vendido o cedido en uso, indicando las características. )
                      v_unit||'|'|| -- 6 (Valor unitario por ítem / precio unitario)
                      CONV_NUM_CAR(ROUND(V_SUMA_TRIBU,2),2)||'|'|| -- 7 SUMATORIA DE TRIBUTOS
                      v_cod_tributo||'|'|| -- #8 (Código de tributo) 1000
                      CONV_NUM_CAR(ROUND(i.A4,2),2)||'|'||  -- #9 (Monto IGV por ítem)
                      CONV_NUM_CAR(ROUND(i.A11,2),2)||'|'|| -- 10 Tributo: Base Imponible IGV por Item
                      v_nom_tributo||'|'||   -- #11 (Tributo: Nombre de tributo por item)
                      v_valor_tributo||'|'||   -- #12 (Código de tributo)                    
                      i.A7||'|'||   -- #13 AFECTACION AL IGV POR ITEM
                      CONV_NUM_CAR(ROUND(i.A12,2),2)||'|'|| --#14 (Porcentaje del impuesto)                     
                      --======== IMPUESTO ISC NO EXISTE EN HSIC ==============================================
                      '-'||'|'|| -- #15
                      '0.00'||'|'|| -- #16
                      '0.00'||'|'|| -- #17
                      ''||'|'|| -- #18
                      ''||'|'|| -- #19
                      ''||'|'|| -- #20
                      '0.00'||'|'|| -- #21
                      --==============================================================================================
                      '-'||'|'|| -- #22
                      '0.00'||'|'|| -- #23
                      '0.00'||'|'|| -- #24
                      ''||'|'|| -- #25
                      ''||'|'|| -- #26
                      '0.00'||'|'|| -- #27
                      --======== Tributo ICBPER ==============================================                                       
                      '-'||'|'||
                      '0.00'||'|'||
                      '0'||'|'||                        
                      'ICBPER'||'|'||
                      'OTH'||'|'||
                      '0.00'||'|'||
                      p_venta||'|'|| -- #28 (precio de venta unitario)
                      CONV_NUM_CAR(ROUND(i.A11,2),2)||'|'|| -- #29 (valor de venta x item)
                      '0.00'); -- #30     
    END LOOP;
    UTL_FILE.FCLOSE(Archivo);

    --========================================== ARCHIVO: LEYENDAS =================================================
    -- CREACION DEL ARCHIVO LEYENDAS 
    Archivo := UTL_FILE.FOPEN(kDirSfs14, RQ||'.LEY', 'W', 32767);
    UTL_FILE.PUT_LINE(Archivo,'1000'||'|'|| --1 (Código de leyenda)
                FACTU.LETRAS(REC_ARFAFE.TOTAL,REC_ARFAFE.MONEDA) --2 (Descripcion de leyenda)
                ); --#161 (Separador de linea)
    UTL_FILE.FCLOSE(Archivo);
 
    --========================================== ARCHIVO: ADICIONALES DE CABECERA  ================================
    Archivo := UTL_FILE.FOPEN(kDirSfs14, RQ||'.ACA', 'W', 32767);
               UTL_FILE.PUT_LINE(Archivo,''||'|'|| --#1 Cuenta de banco - Detracciones
                ''||'|'|| --#2
                ''||'|'||--#3
                ''||'|'|| --#4 Monto de la detraccion
                ''||'|'|| --#5 Medio de pago                
                --Direccion Del Cliente
                COD_PAIS_SUNAT_CLIENTE||'|'|| --#6 (Código de país)
                UBIGEO_CLIENTE||'|'|| --#7 (Código de ubigeo)
                SUBSTR(DIRECCION_CLIENTE,1,200)||'|'|| --#8 (Dirección completa y detallada)                
                ''||'|'|| --#9
                ''||'|'|| --#10
                '-'   --#11 
                ); --(Separador de linea)
    UTL_FILE.FCLOSE(Archivo);

    --=====================================ARCHIVO: ADICIONALES DEL DETALLES =======================================  
    Archivo := UTL_FILE.FOPEN(kDirSfs14, RQ||'.ADE', 'W', 32767);
    -----------------------------------------  DATALLES ------------------------------------------------------------
    FOR i IN C_DET_ITEMS LOOP
                IF i.A9 > 0 then
                        XDESC:='false';
                        catalogo_desc_item := COD_SUNAT_DESC_GLOBAL(53,'I');
                        fact_desc_item := CONV_NUM_CAR(ROUND(i.A10,2)/100,2);
                        mont_desc_item := CONV_NUM_CAR(i.A9,2);
                        mont_base_desc_item := ROUND(i.A2,2);
                        
                        BEGIN
                        SELECT CODIGO
                        INTO MONEDA
                        FROM TABLAS_SUNAT_FE
                        WHERE CLASE = '2' AND 
                              VALOR = REC_ARFAFE.MONEDA;  
                        EXCEPTION WHEN NO_DATA_FOUND THEN null;
                        END;
                ELSE
                        XDESC:='-';
                        catalogo_desc_item := '';
                        fact_desc_item :='0';
                        mont_desc_item := '0';
                        mont_base_desc_item := '0';
                        MONEDA:='';
                END IF;
                UTL_FILE.PUT_LINE(Archivo,i.A8||'|'|| -- 1 (Número de línea)
                        --Para las Detracciones
                        '-'||'|'|| --2 Nombre del item
                        ''||'|'|| --3
                        ''||'|'|| --4
                        '-'||'|'|| --5
                        '-'||'|'|| --6
                        '-'||'|'|| --7
                        '-'||'|'|| --8
                        '-'||'|'|| --9                        
                        XDESC||'|'||              --10 (Tipo de Variable)
                        catalogo_desc_item||'|'|| --11 (Código de tipo del ítem)
                        fact_desc_item||'|'||     --12 (Porcentaje del ítem)
                        MONEDA||'|'||             --13 (Moneda de monto del item)
                        mont_desc_item||'|'||     --14 (Monto del ítem)  
                        MONEDA||'|'||             --15 (Moneda de base imponible del item)
                        mont_base_desc_item       --16 (Base imponible del ítem)
                        );
    END LOOP;
    UTL_FILE.FCLOSE(Archivo); 
    --=================================================================================================
    --CREDITOS O CUOTAS (Solo para facturas)
    IF V_TIPO_DOC_CHAR = 'F' AND SUBSTR(substr(REC_ARFAFE.no_factu,1,4),0,1) IN('F') THEN
       V_FP := FACTU.GET_FORMA_PAGO(P_NO_CIA,REC_ARFAFE.TIPO_FPAGO,REC_ARFAFE.COD_FPAGO);
       
       IF V_FP IN('CRE') THEN
         CREAR_ARCHIVO_CUOTA(RQ,P_NO_CIA, REC_ARFAFE.NO_CLIENTE, REC_ARFAFE.no_factu, MONEDA);
       ELSIF V_FP IN ('CON') THEN
        IF REC_ARFAFE.MONEDA IN('SOL') THEN
          MONEDA := 'PEN';
        ELSE
          MONEDA := 'USD';
        END IF;
        Archivo := UTL_FILE.FOPEN(kDirSfs14, RQ||'.PAG', 'W', 32767);
        UTL_FILE.PUT_LINE(Archivo,'Contado'||'|'|| -- 1 Forma de pago                        
        CONV_NUM_CAR(ROUND(nvl(REC_ARFAFE.TOTAL,0),2),2)||'|'|| --2 Monto neto pendiente de pago                       
        MONEDA--3 MONEDA
        );
        UTL_FILE.FCLOSE(Archivo);
       END IF;
    END IF;    
    -- FIN DE CUOTAS O CREDITO

EXCEPTION
  WHEN OTHERS THEN
        RAISE_APPLICATION_ERROR(-20001,'No se pudo crear Archivo de Facturación Electrónica...!!!');
END CREAR_ARCHIVO_SFS;

   /*---------------------------------------------------------------------------------------
   Nombre      : CREAR_ARCHIVO_CUOTA
   Proposito   : PROCEDIMIENTO QUE NOS PERMITE CREAR ARCHIVO PLANO PARA CUOTAS
   Parametro  :

   Log de Cambios:
     Fecha        Autor                     Descripción
     05/10/2025   Robinzon Santana          Creador   
  -----------------------------------------------------------------------------------------*/
  PROCEDURE CREAR_ARCHIVO_CUOTA(
    P_RQ IN VARCHAR2,
    P_NO_CIA IN FACTU.ARPFOE.NO_CIA%TYPE,
    P_NO_CLIENTE IN FACTU.ARFAFE.NO_CLIENTE%TYPE,  
    P_NO_FACTU IN FACTU.ARFAFE.NO_FACTU%TYPE,
    P_MONEDA IN FACTU.ARFAFE.MONEDA%TYPE
  ) IS
     Archivo UTL_FILE.FILE_TYPE;
 
      CURSOR DET_CUOTAS IS
        SELECT NO_CREDITO a1, CONV_NUM_CAR(ROUND(nvl(MONTO,0),2),2) a2,TO_CHAR(FEC_PAGO,'yyyy-mm-dd') a3
        FROM FACTU.ARFCRED
        WHERE NO_CIA = P_NO_CIA
        AND NO_CLIENTE = P_NO_CLIENTE
        AND NO_ORDEN = P_NO_FACTU
        ORDER BY NO_CREDITO;
        
        V_MONEDA VARCHAR2(5);
        kDirSfs14 CONSTANT VARCHAR2(10) := 'DIR_SFS_14';
  BEGIN
      
      IF P_MONEDA IN('SOL') THEN
        V_MONEDA := 'PEN';
      ELSE
        V_MONEDA := 'USD';
      END IF;
      
       -- Archivo := TEXT_IO.FOPEN(RUTA||'.PAG', 'W');
       Archivo := UTL_FILE.FOPEN(kDirSfs14, P_RQ||'.PAG', 'W', 32767);
       UTL_FILE.PUT_LINE(Archivo,'Credito'||'|'|| -- 1 Forma de pago                        
                   RTRIM(LTRIM(FACTU.MONTO_CUOTA(P_NO_CIA,P_NO_CLIENTE,P_NO_FACTU)))||'|'|| --2 Monto neto pendiente de pago                       
                   V_MONEDA--3 MONEDA
                   );
       UTL_FILE.FCLOSE(Archivo);
       
       -- Archivo := TEXT_IO.FOPEN(RUTA||'.DPA', 'W');
       Archivo := UTL_FILE.FOPEN(kDirSfs14, P_RQ||'.DPA', 'W', 32767);
       
        FOR i IN DET_CUOTAS LOOP
            UTL_FILE.PUT_LINE(Archivo,RTRIM(LTRIM(i.a2))||'|'|| -- 1 Monto(s) del pago unico o de las cuotas
                                RTRIM(LTRIM(i.a3))||'|'|| --2 Fecha(s) de vencimiento del pago unico o de las cuotas
                                V_MONEDA--3 MONEDA
                                );
         END LOOP;
         UTL_FILE.FCLOSE(Archivo);
    
  EXCEPTION
    WHEN OTHERS THEN
          RAISE_APPLICATION_ERROR(-20001,'No se pudo crear Archivo de COUTAS'); 
  END CREAR_ARCHIVO_CUOTA;
  
     /*---------------------------------------------------------------------------------------
   Nombre      : EXISTE_DOCUMENTO
   Proposito   : EXISTE EL DOCUMENTO DE PAGO
   Parametro  :

   Log de Cambios:
     Fecha        Autor                     Descripción
     04/02/2026   Robinzon Santana          Creador   
  -----------------------------------------------------------------------------------------*/
  FUNCTION EXISTE_DOCUMENTO(P_NO_CIA IN FACTU.ARFAFE.NO_CIA%TYPE,
                            P_TIPO_DOC IN FACTU.ARFAFE.TIPO_DOC%TYPE,  
                            P_NO_FACTU IN FACTU.ARFAFE.NO_FACTU%TYPE ) RETURN CHAR
  IS
    cExiste CHAR(1);
    cNoExiste CONSTANT CHAR(1) := 'N';
  BEGIN
       SELECT 'S'
       INTO cExiste
       FROM FACTU.ARFAFE
       WHERE NO_CIA = P_NO_CIA
       --AND TIPO_DOC = P_TIPO_DOC
       AND NO_FACTU = P_NO_FACTU;
       
       RETURN cExiste;
  
  EXCEPTION
    WHEN OTHERS THEN
      cExiste :=  cNoExiste;
      return cExiste;
    
  END EXISTE_DOCUMENTO;
  
  
   /*---------------------------------------------------------------------------------------
   Nombre      : ANULAR_DOC_PAGO
   Proposito   : PROCEDIMIENTO QUE NOS PERMITE ANULAR DOCUMENTOS DE PAGO
   Parametro  :

   Log de Cambios:
     Fecha        Autor                     Descripción
     04/02/2026   Robinzon Santana          Creador   
  -----------------------------------------------------------------------------------------*/
  PROCEDURE ANULAR_DOC_PAGO(
            P_NO_CIA IN FACTU.ARFAFE.NO_CIA%TYPE,
            P_TIPO_DOC IN FACTU.ARFAFE.TIPO_DOC%TYPE,
            P_NO_FACTU IN FACTU.ARFAFE.NO_FACTU%TYPE,
            P_TIP_ANULAR IN FACTU.ARFAFE.TIP_DOC_ANULAR%TYPE,
            P_NUM_ANULAR IN FACTU.ARFAFE.NUM_ANULAR%TYPE,
            P_MOT_ANULAR IN FACTU.ARFAFE.MOT_ANULAR%TYPE,
            p_cMensaje OUT VARCHAR2
             ) 
  IS
    cExiste CHAR(1);
    cNoExiste CONSTANT CHAR(1) := 'N';
  BEGIN
     cExiste := EXISTE_DOCUMENTO(P_NO_CIA, P_TIPO_DOC, P_NO_FACTU);
     
     IF NVL(cExiste,cNoExiste) != cNoExiste THEN

         UPDATE FACTU.ARFAFE
         SET TIP_DOC_ANULAR = P_TIP_ANULAR,
             NUM_ANULAR = P_NUM_ANULAR,
             MOT_ANULAR = P_MOT_ANULAR,
             ESTADO = 'A'
         WHERE NO_CIA = P_NO_CIA
         AND TIPO_DOC = P_TIPO_DOC
         AND NO_FACTU = P_NO_FACTU;
         
         COMMIT;
         
         p_cMensaje := 'Se anulo el documento '||P_NO_FACTU;
         
     ELSE
        p_cMensaje := 'No se pudo anular el documento '||P_NO_FACTU;  
     END IF;
    
  END ANULAR_DOC_PAGO;
  
  /*---------------------------------------------------------------------------------------
   Nombre      : ACTU_ESTADO_ENVIO
   Proposito   : PROCEDIMIENTO QUE NOS PERMITE CAMBIAR EL ESTADO Y LA FECHA DE ENVIO A SUNAT
   Parametro  :

   Log de Cambios:
     Fecha        Autor                     Descripción
     04/02/2026   Robinzon Santana          Creador   
  -----------------------------------------------------------------------------------------*/
  PROCEDURE ACTU_ESTADO_ENVIO(
            P_NO_CIA IN FACTU.ARFAFE.NO_CIA%TYPE,
            P_TIPO_DOC IN FACTU.ARFAFE.TIPO_DOC%TYPE,  
            P_NO_FACTU IN FACTU.ARFAFE.NO_FACTU%TYPE,
            P_PROC_STATUS IN FACTU.ARFAFE.PROCE_STATUS%TYPE
    ) 
  IS
    cExiste CHAR(1);
    cNoExiste CONSTANT CHAR(1) := 'N';
  BEGIN
    cExiste := EXISTE_DOCUMENTO(P_NO_CIA, P_TIPO_DOC, P_NO_FACTU);
    
    IF NVL(cExiste,cNoExiste) != cNoExiste THEN

         UPDATE FACTU.ARFAFE
         SET PROCE_STATUS = P_PROC_STATUS,
             PROCE_FECHA = SYSDATE
         WHERE NO_CIA = P_NO_CIA
         AND TIPO_DOC = P_TIPO_DOC
         AND NO_FACTU = P_NO_FACTU;
         
         COMMIT;
 
    END IF;
    
  END ACTU_ESTADO_ENVIO;
  
    /*---------------------------------------------------------------------------------------
   Nombre      : REG_COMUNI_BAJA
   Proposito   : PROCEDIMIENTO QUE NOS VA PERMITIR DAR LA COMUNICACIÓN DE BAJA DE UN COMPROBANTE PAGO
   Parametro  :

   Log de Cambios:
     Fecha        Autor                     Descripción
     13/02/2026   Robinzon Santana          Creador   
  -----------------------------------------------------------------------------------------*/
   PROCEDURE REG_COMUNI_BAJA(
      pNoCia IN FACTU.ARFAFE.NO_CIA%TYPE,
      pNoFactu IN FACTU.ARFAFE.NO_FACTU%TYPE,
      pFecBaja IN FACTU.COMUNICACION_BAJA.FEC_BAJA%TYPE,
      pCodMotivo IN FACTU.COMUNICACION_BAJA.COD_MOTIVO%TYPE,
      pDescMotivo IN FACTU.COMUNICACION_BAJA.DESC_MOTIVO%TYPE
    )
   IS
     
     cExiste CHAR(1);
     dFecEmision FACTU.COMUNICACION_BAJA.FEC_EMISION%TYPE;
     dFecBaja    FACTU.COMUNICACION_BAJA.FEC_BAJA%TYPE;
     nCantFecEmi NUMBER;
     cNroCorre   FACTU.COMUNICACION_BAJA.NRO_CORRELATIVO%TYPE;
     
   BEGIN
     cExiste := EXISTE_DOCUMENTO(pNoCia, NULL, pNoFactu);
     
     IF cExiste = 'S' THEN
         
         IF pFecBaja IS NULL THEN
            dFecBaja := TRUNC(SYSDATE);
         ELSE
            dFecBaja := pFecBaja;
         END IF;
         
         BEGIN
           SELECT TRUNC(FECHA) AS FEC_EMISION
           INTO dFecEmision
           FROM FACTU.ARFAFE
           WHERE NO_CIA = pNoCia
           AND NO_FACTU = pNoFactu;
         EXCEPTION
           WHEN OTHERS THEN
              dFecEmision := NULL;
         END;
         
         
         BEGIN
           SELECT COUNT(*)
           INTO nCantFecEmi
           FROM FACTU.COMUNICACION_BAJA
           WHERE FEC_EMISION = dFecEmision
           AND SUBSTR(NO_FACTU,1,1) NOT IN('B','b');
           
           nCantFecEmi := nCantFecEmi + 1;
         EXCEPTION
           WHEN OTHERS THEN
             nCantFecEmi := 0;           
         END;
         
         cNroCorre := TO_CHAR(nCantFecEmi);
         
         INSERT INTO FACTU.COMUNICACION_BAJA (NO_CIA, NO_FACTU, FEC_EMISION, FEC_BAJA,
                                              COD_MOTIVO, DESC_MOTIVO, NRO_CORRELATIVO) 
                                              VALUES (pNoCia, pNoFactu, dFecEmision, dFecBaja,
                                              pCodMotivo, pDescMotivo, cNroCorre);
                                              
         UPDATE FACTU.ARFAFE
         SET ESTADO = 'A'
         WHERE NO_CIA = pNoCia
         AND NO_FACTU = pNoFactu;
         
         COMMIT;
            
     END IF;
     
   END REG_COMUNI_BAJA;
   
 /*---------------------------------------------------------------------------------------
   Nombre      : REG_RESUM_DIARIO
   Proposito   : PROCEDIMIENTO QUE NOS PERMITA REGISTRAR UN RESUMEN DIARIO
   Parametro  :

   Log de Cambios:
     Fecha        Autor                     Descripción
     23/02/2026   Robinzon Santana          Creador   
 -----------------------------------------------------------------------------------------*/
  PROCEDURE REG_RESUM_DIARIO(
     pNoCia IN FACTU.T_RESUMEN_DIARIO.NO_CIA%TYPE,
     pFecEmisor IN FACTU.T_RESUMEN_DIARIO.FEC_EMISION%TYPE,
     pRucEmisor IN FACTU.T_RESUMEN_DIARIO.RUC_EMISOR%TYPE,
     pTicket  IN FACTU.T_RESUMEN_DIARIO.TICKET%TYPE,
     pDescrip IN FACTU.T_RESUMEN_DIARIO.DESCRIPCION%TYPE
   ) IS
   
    cCorrResDia VARCHAR2(5);
   
   BEGIN
      
      cCorrResDia := GET_CORRE_RESDIA(pNoCia, pFecEmisor);
      
      INSERT INTO FACTU.T_RESUMEN_DIARIO( NO_CIA, FEC_EMISION, NRO_CORRELATIVO, RUC_EMISOR,
                                          TICKET, DESCRIPCION )
                                         VALUES (pNoCia, pFecEmisor, cCorrResDia, pRucEmisor,
                                           pTicket, pDescrip );
                                          
      COMMIT;                                          
     
   EXCEPTION
     WHEN OTHERS THEN
        ROLLBACK;
   END REG_RESUM_DIARIO;
   
 /*---------------------------------------------------------------------------------------
   Nombre      : GET_CORRE_RESDIA
   Proposito   : FUNCION PARA OBTENER EL CORRELATIVO DE UN RESUMEN DIARIO
   Parametro  :

   Log de Cambios:
     Fecha        Autor                     Descripción
     23/02/2026   Robinzon Santana          Creador   
 -----------------------------------------------------------------------------------------*/
  FUNCTION GET_CORRE_RESDIA( pNoCia IN FACTU.T_RESUMEN_DIARIO.NO_CIA%TYPE,
                            pFecEmisor IN FACTU.T_RESUMEN_DIARIO.FEC_EMISION%TYPE ) RETURN VARCHAR2
  IS
    nCorrResDia NUMBER;
  BEGIN
    
    SELECT COUNT(FEC_EMISION)
    INTO nCorrResDia
    FROM FACTU.T_RESUMEN_DIARIO
    WHERE NO_CIA = pNoCia
    AND FEC_EMISION = pFecEmisor;
    
    nCorrResDia := nCorrResDia + 1;
    
    RETURN TO_CHAR(nCorrResDia);
    
  EXCEPTION
     WHEN OTHERS THEN
       RETURN '0';
  
  END GET_CORRE_RESDIA;
   
END PR_FACTURA;
/