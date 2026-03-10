CREATE OR REPLACE PACKAGE BODY CXC.PR_CLIENTE IS

   /*---------------------------------------------------------------------------------------
   Nombre      : GUARDAR_CLIENTE
   Proposito   : PROCEDIMIENTO QUE NOS PERMITE ACTUALIZAR O REGISTRAR CLIENTE
   Parametro  :

   Log de Cambios:
     Fecha        Autor                     Descripción
     13/01/2026   Robinzon Santana          Creador
  -----------------------------------------------------------------------------------------*/
 PROCEDURE GUARDAR_CLIENTE(p_cNoCia IN CXC.ARCCMC.NO_CIA%TYPE,
                   p_cNoCliente IN CXC.ARCCMC.NO_CLIENTE%TYPE,
                   p_cNombre IN CXC.ARCCMC.NOMBRE%TYPE,
                   p_cCelular IN CXC.ARCCMC.TELEFONO1%TYPE,
                   p_cTelefono IN CXC.ARCCMC.TELEFONO2%TYPE,
                   p_cTipoPersona IN CXC.ARCCMC.TIPO_PERSONA%TYPE,
                   p_cExtranjero IN CXC.ARCCMC.EXTRANJERO%TYPE,
                   p_cActivo IN CXC.ARCCMC.ACTIVO%TYPE,
                   p_cTipoDocumento IN CXC.ARCCMC.TIPO_DOCUMENTO%TYPE,
                   p_cEmail IN CXC.ARCCMC.EMAIL%TYPE,
                   p_cDireccion IN CXC.ARCCTDA.DIRECCION%TYPE,
                   p_cCodiDepa IN CXC.ARCCTDA.CODI_DEPA%TYPE,
                   p_cCodiProv IN CXC.ARCCTDA.CODI_PROV%TYPE,
                   p_cCodiDist IN CXC.ARCCTDA.CODI_DIST%TYPE
                 ) IS

    cRuc  CXC.ARCCMC.RUC%TYPE;
    cNuDocumento  CXC.ARCCMC.NU_DOCUMENTO%TYPE;

  BEGIN

      IF p_cTipoPersona = 'J' THEN
           cRuc := p_cNoCliente;
           cNuDocumento := NULL;
      ELSIF p_cTipoPersona = 'N' THEN
           cRuc := NULL;
           cNuDocumento := p_cNoCliente;
      END IF;
      
      UPDATE CXC.ARCCMC
      SET NOMBRE = p_cNombre,
          TELEFONO1 = p_cCelular,
          TELEFONO2 = p_cTelefono,
          RUC = cRuc,
          NU_DOCUMENTO = cNuDocumento,
          TIPO_CLIENTE = 'B',
          GRUPO = '00',
          USUARIO = USER,
          TIPO_PERSONA = p_cTipoPersona,
          ACTIVO = p_cActivo,
          TIPO_DOCUMENTO = p_cTipoDocumento,
          EMAIL = p_cEmail,
          COD_SUC = '001',
          EXTRANJERO = p_cExtranjero
      WHERE NO_CIA = p_cNoCia
      AND NO_CLIENTE = p_cNoCliente;
      
      IF SQL%ROWCOUNT = 0 THEN
          INSERT INTO CXC.ARCCMC (
              NO_CIA, NO_CLIENTE, NOMBRE , 
              RUC, TELEFONO1 , TELEFONO2 ,
              TIPO_CLIENTE, EXTRANJERO, TIPO_PERSONA,
              ACTIVO, COD_PAIS, TIPO_DOCUMENTO, 
              EMAIL, COD_SUC, NU_DOCUMENTO
             ) VALUES (
               p_cNoCia, p_cNoCliente, p_cNombre,
               cRuc, p_cCelular, p_cTelefono,
               'B', p_cExtranjero, p_cTipoPersona,
               p_cActivo, '001', p_cTipoDocumento,
               p_cEmail, '001', cNuDocumento
             );
      END IF;
      
      -- DIRECCION  
      GUARDAR_DIRECCION(p_cNoCia, p_cNoCliente, p_cDireccion,
                        p_cCodiDepa, p_cCodiProv, p_cCodiDist,
                        p_cActivo
                       );  
      
      COMMIT;
      
  EXCEPTION
       WHEN OTHERS THEN
          RAISE_APPLICATION_ERROR(-20000, 'CXC.PR_CLIENTE.GUARDAR_CLIENTE => '||SQLERRM);
          ROLLBACK;

  END GUARDAR_CLIENTE;

   /*---------------------------------------------------------------------------------------
   Nombre      : GUARDAR_DIRECCION
   Proposito   : PROCEDIMIENTO QUE NOS PERMITE ACTUALIZAR O REGISTRAR DIRECCION
   Parametro  :

   Log de Cambios:
     Fecha        Autor                     Descripción
     14/01/2026   Robinzon Santana          Creador
  -----------------------------------------------------------------------------------------*/
  PROCEDURE GUARDAR_DIRECCION(p_cNoCia IN CXC.ARCCTDA.NO_CIA%TYPE,
                   p_cNoCliente IN CXC.ARCCTDA.NO_CLIENTE%TYPE,
                   p_cDireccion IN CXC.ARCCTDA.DIRECCION%TYPE,
                   p_cCodiDepa IN CXC.ARCCTDA.CODI_DEPA%TYPE,
                   p_cCodiProv IN CXC.ARCCTDA.CODI_PROV%TYPE,
                   p_cCodiDist IN CXC.ARCCTDA.CODI_DIST%TYPE,
                   p_cActivo IN CXC.ARCCTDA.ACTIVO%TYPE
                 ) IS
                 
  BEGIN
    
     UPDATE CXC.ARCCTDA
     SET DIRECCION = p_cDireccion,
         CODI_DEPA = p_cCodiDepa,
         CODI_PROV = p_cCodiProv,
         CODI_DIST = p_cCodiDist
      WHERE NO_CIA = p_cNoCia
      AND NO_CLIENTE = p_cNoCliente
      AND COD_TIENDA = '001'; -- PRINCIPAL
      
     IF SQL%ROWCOUNT = 0 THEN
       
        INSERT INTO CXC.ARCCTDA (
           NO_CIA, NO_CLIENTE, COD_TIENDA , NOMBRE,
           DIRECCION, CODI_DEPA, CODI_PROV, CODI_DIST,
           TIPO_DIR, ACTIVO, TIPO_ENTI, COD_SUC,
           ESTAB_SUNAT
         ) VALUES (
            p_cNoCia, p_cNoCliente, '001', 'LEGAL',
            p_cDireccion, p_cCodiDepa, p_cCodiProv, p_cCodiDist,
            'LEG', p_cActivo, 'C', '001',
            '0000'
         );
       
     END IF;
  
  END GUARDAR_DIRECCION;
  
  /*---------------------------------------------------------------------------------------
   Nombre      : GET_DIRECCION
   Proposito   : FUNCION PARA CAPTURAR LA DIRECCION DEL CLIENTE CON LA DESCRIPCION DE DEPARTAMENTO-PROVINCIA-DISTRITO
   Parametro  :

   Log de Cambios:
     Fecha        Autor                     Descripción
     16/01/2026   Robinzon Santana          Creador
  -----------------------------------------------------------------------------------------*/
  FUNCTION GET_DIRECCION(p_cNoCia IN CXC.ARCCTDA.NO_CIA%TYPE,
                         p_cNoCliente IN CXC.ARCCTDA.NO_CLIENTE%TYPE) RETURN VARCHAR2
  
  IS
  
     kIsNulo CONSTANT CHAR(1) := 'S';
     nPosDepa   NUMBER;
     nPosProv   NUMBER;
     nPosDist   NUMBER;
     
     cDireccion CXC.ARCCTDA.DIRECCION%TYPE;
     cDescDepa CXC.ARCCDP.DESC_DEPA%TYPE;
     cDescProv CXC.ARCCPR.DESC_PROV%TYPE;
     cDescDist CXC.ARCCDI.DESC_DIST%TYPE;
  
  BEGIN
      BEGIN
          SELECT CD.DIRECCION, D.DESC_DEPA, P.DESC_PROV, DI.DESC_DIST
          INTO cDireccion, cDescDepa, cDescProv, cDescDist
          FROM CXC.ARCCTDA CD, CXC.ARCCDP D, CXC.ARCCPR P, CXC.ARCCDI DI
          WHERE CD.NO_CIA = p_cNoCia
          AND CD.NO_CLIENTE = p_cNoCliente
          AND D.NO_CIA = CD.NO_CIA
          AND D.CODI_DEPA = CD.CODI_DEPA
          AND P.NO_CIA = CD.NO_CIA
          AND P.CODI_DEPA = CD.CODI_DEPA
          AND P.CODI_PROV = CD.CODI_PROV
          AND DI.NO_CIA = CD.NO_CIA
          AND DI.CODI_DEPA = CD.CODI_DEPA
          AND DI.CODI_PROV = CD.CODI_PROV
          AND DI.CODI_DIST = CD.CODI_DIST;
      EXCEPTION
         WHEN OTHERS THEN
             cDireccion := NULL;
             cDescDepa := NULL;
             cDescProv := NULL;
             cDescDist := NULL;
      END;
      
      IF NVL(cDireccion, 'S') = kIsNulo THEN
         cDireccion := '-----';
         RETURN cDireccion;
      END IF;
      
       -- DEPARTAMENTO
    IF cDescDepa IS NOT NULL AND INSTR(cDireccion, UPPER(cDescDepa)) = 0 THEN
        cDireccion := cDireccion || ' ' || UPPER(cDescDepa)|| ' - ' || UPPER(cDescProv)|| ' - ' || UPPER(cDescDist);
        RETURN cDireccion;
    END IF;
    
    nPosDepa := INSTR(cDireccion, UPPER(cDescDepa));
    nPosProv := INSTR(cDireccion, UPPER(cDescProv));
    nPosDist := INSTR(cDireccion, UPPER(cDescDist));
    
    DBMS_OUTPUT.put_line('NO_CLIENTE = '||p_cNoCliente||' ; nPosDepa = '||nPosDepa||'  ,  nPosProv = '||nPosProv|| ' , nPosDist = '||nPosDist);
    
    IF nPosDepa > 0 THEN
        DBMS_OUTPUT.put_line('SE ENCONTRO cDescDepa => nPosDepa = '||nPosDepa||'  ,  cDescDepa ='||cDescDepa);
        cDireccion := TRIM( SUBSTR(cDireccion, 1, nPosDepa - 1) );
      
        cDireccion := cDireccion||' '|| UPPER(cDescDepa)|| ' - ' || UPPER(cDescProv)|| ' - ' || UPPER(cDescDist);
        RETURN cDireccion;
    END IF;
     
    RETURN cDireccion;
      
  END GET_DIRECCION;
  
    /*---------------------------------------------------------------------------------------
   Nombre      : GET_DIRECC_CIA
   Proposito   : FUNCION PARA OBTENER LA DIRECCION DE LA EMPRESA
   Parametro  :

   Log de Cambios:
     Fecha        Autor                     Descripción
     10/03/2026   Robinzon Santana          Creador
  -----------------------------------------------------------------------------------------*/
  FUNCTION GET_DIRECC_CIA(p_cNoCia IN FACTU.SUCURSAL_PTOVTA.NO_CIA%TYPE) RETURN VARCHAR2
  IS
     
     kIsNulo CONSTANT CHAR(1) := 'S';
     
     nPosDepa   NUMBER;
     nPosProv   NUMBER;
     nPosDist   NUMBER;
     
     cDireccion CXC.ARCCTDA.DIRECCION%TYPE;
     cDescDepa CXC.ARCCDP.DESC_DEPA%TYPE;
     cDescProv CXC.ARCCPR.DESC_PROV%TYPE;
     cDescDist CXC.ARCCDI.DESC_DIST%TYPE;
  
  BEGIN
      BEGIN
          SELECT CD.DIRECCION, D.DESC_DEPA, P.DESC_PROV, DI.DESC_DIST
          INTO cDireccion, cDescDepa, cDescProv, cDescDist
          FROM FACTU.SUCURSAL_PTOVTA CD, CXC.ARCCDP D, CXC.ARCCPR P, CXC.ARCCDI DI
          WHERE CD.NO_CIA = p_cNoCia
          AND D.NO_CIA = CD.NO_CIA
          AND D.CODI_DEPA = CD.CODI_DEPA
          AND P.NO_CIA = CD.NO_CIA
          AND P.CODI_DEPA = CD.CODI_DEPA
          AND P.CODI_PROV = CD.CODI_PROV
          AND DI.NO_CIA = CD.NO_CIA
          AND DI.CODI_DEPA = CD.CODI_DEPA
          AND DI.CODI_PROV = CD.CODI_PROV
          AND DI.CODI_DIST = CD.CODI_DIST;
      EXCEPTION
         WHEN OTHERS THEN
             cDireccion := NULL;
             cDescDepa := NULL;
             cDescProv := NULL;
             cDescDist := NULL;
      END;
      
      IF NVL(cDireccion, 'S') = kIsNulo THEN
         cDireccion := '-----';
         RETURN cDireccion;
      END IF;
    cDireccion := cDireccion || ' ' || UPPER(cDescDepa)|| ' - ' || UPPER(cDescProv)|| ' - ' || UPPER(cDescDist);
    
       -- DEPARTAMENTO
    IF cDescDepa IS NOT NULL AND INSTR(cDireccion, UPPER(cDescDepa)) = 0 THEN
        cDireccion := cDireccion || ' ' || UPPER(cDescDepa)|| ' - ' || UPPER(cDescProv)|| ' - ' || UPPER(cDescDist);
        RETURN cDireccion;
    END IF;
    
    nPosDepa := INSTR(cDireccion, UPPER(cDescDepa));
    nPosProv := INSTR(cDireccion, UPPER(cDescProv));
    nPosDist := INSTR(cDireccion, UPPER(cDescDist));
    
    IF nPosDepa > 0 THEN
        DBMS_OUTPUT.put_line('SE ENCONTRO cDescDepa => nPosDepa = '||nPosDepa||'  ,  cDescDepa ='||cDescDepa);
        cDireccion := TRIM( SUBSTR(cDireccion, 1, nPosDepa - 1) );
      
        cDireccion := cDireccion||' '|| UPPER(cDescDepa)|| ' - ' || UPPER(cDescProv)|| ' - ' || UPPER(cDescDist);
        RETURN cDireccion;
    END IF;
    
     
    RETURN cDireccion;
      
  END GET_DIRECC_CIA; 

END PR_CLIENTE;
