CREATE OR REPLACE PACKAGE CXC.PR_CLIENTE IS

 -- PROCEDIMIENTO QUE NOS PERMITE ACTUALIZAR O REGISTRAR CLIENTE
 PROCEDURE GUARDAR_CLIENTE(p_cNoCia IN CXC.ARCCMC.NO_CIA%TYPE,
                   p_cNoCliente IN CXC.ARCCMC.NO_CLIENTE%TYPE,
                   p_cNombre IN CXC.ARCCMC.NOMBRE%TYPE,
                   p_cCelular IN CXC.ARCCMC.TELEFONO1%TYPE,
                   p_cTelefono IN CXC.ARCCMC.TELEFONO2%TYPE,
                   -- p_cRuc IN CXC.ARCCMC.RUC%TYPE,
                   p_cTipoPersona IN CXC.ARCCMC.TIPO_PERSONA%TYPE,
                   -- p_cTipoCliente IN CXC.ARCCMC.TIPO_CLIENTE%TYPE,
                   -- p_cGrupo IN CXC.ARCCMC.GRUPO%TYPE,
                   p_cExtranjero IN CXC.ARCCMC.EXTRANJERO%TYPE,
                   p_cActivo IN CXC.ARCCMC.ACTIVO%TYPE,
                   --p_cCodPais IN CXC.ARCCMC.COD_PAIS%TYPE,
                   p_cTipoDocumento IN CXC.ARCCMC.TIPO_DOCUMENTO%TYPE,
                   p_cEmail IN CXC.ARCCMC.EMAIL%TYPE,
                   --
                   p_cDireccion IN CXC.ARCCTDA.DIRECCION%TYPE,
                   p_cCodiDepa IN CXC.ARCCTDA.CODI_DEPA%TYPE,
                   p_cCodiProv IN CXC.ARCCTDA.CODI_PROV%TYPE,
                   p_cCodiDist IN CXC.ARCCTDA.CODI_DIST%TYPE
                 );

 
 -- PROCEDIMIENTO QUE NOS PERMITE ACTUALIZAR O REGISTRAR DIRECCION
 PROCEDURE GUARDAR_DIRECCION(p_cNoCia IN CXC.ARCCTDA.NO_CIA%TYPE,
                   p_cNoCliente IN CXC.ARCCTDA.NO_CLIENTE%TYPE,
                 --  p_cCodTienda IN CXC.ARCCTDA.COD_TIENDA%TYPE,
                 --  p_cNombre IN CXC.ARCCTDA.NOMBRE%TYPE,
                   p_cDireccion IN CXC.ARCCTDA.DIRECCION%TYPE,
                   p_cCodiDepa IN CXC.ARCCTDA.CODI_DEPA%TYPE,
                   p_cCodiProv IN CXC.ARCCTDA.CODI_PROV%TYPE,
                   p_cCodiDist IN CXC.ARCCTDA.CODI_DIST%TYPE,
                --   p_cTipoDic IN CXC.ARCCTDA.TIPO_DIR%TYPE,
                   p_cActivo IN CXC.ARCCTDA.ACTIVO%TYPE
                --   p_cTipoEnti IN CXC.ARCCTDA.TIPO_ENTI%TYPE,
                --   p_cCodSuc IN CXC.ARCCTDA.COD_SUC%TYPE,
                --   p_cEstabSunat IN CXC.ARCCTDA.ESTAB_SUNAT%TYPE
                 );
                 
 -- FUNCION PARA CAPTURAR LA DIRECION DEL CLIENTE
 FUNCTION GET_DIRECCION(p_cNoCia IN CXC.ARCCTDA.NO_CIA%TYPE,
                   p_cNoCliente IN CXC.ARCCTDA.NO_CLIENTE%TYPE) RETURN VARCHAR2;
                   
 -- FUNCION PARA OBTENER LA DIRECCION DE LA EMPRESA
 FUNCTION GET_DIRECC_CIA(p_cNoCia IN FACTU.SUCURSAL_PTOVTA.NO_CIA%TYPE) RETURN VARCHAR2;

END PR_CLIENTE;
