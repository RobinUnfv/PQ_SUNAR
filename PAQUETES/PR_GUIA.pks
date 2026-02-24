CREATE OR REPLACE PACKAGE FACTU.PR_GUIA IS

  -- PROCEDIMIENTO QUE NOS VA PERMITIR GENERAR UN GUIA FICTA
  FUNCTION GET_GUIA_FICTA(p_cNocia IN FACTU.ARFACF.NO_CIA%TYPE,
                           p_cCentro IN FACTU.ARFACF.CENTRO%TYPE,
                           p_cError IN OUT VARCHAR2
                           ) RETURN FACTU.ARFAFE.NO_GUIA%TYPE;
  
  -- PROCEDIMIENTO QUE ACTUALIZA LA GUIA FICTA
  PROCEDURE UPDATE_GUIA_FICTA(p_cNocia IN FACTU.ARFACF.NO_CIA%TYPE,
                           p_cCentro IN FACTU.ARFACF.CENTRO%TYPE,
                           p_cError IN OUT VARCHAR2);
  
  -- FUNCION QUE NOS DEVUELTE EL NO_DOCU DE LA GUIA
  FUNCTION GET_NO_DOCU( p_cNocia IN INVE.ARINSE.NO_CIA%TYPE,
                        p_cBodega IN INVE.ARINSE.BODEGA%TYPE,
                        p_cTipoDoc IN INVE.ARINSE.TIPO_DOC%TYPE,
                        p_cError IN OUT VARCHAR2
                         ) RETURN INVE.ARINSE.SECUENCIA%TYPE;
  
  -- PROCEDIMIENTO QUE NOS VA PERMITIR ACTUALIZAR LA SECUENCIA DEL NO_DOCU
  PROCEDURE UPDATE_NO_DOCU( p_cNocia IN INVE.ARINSE.NO_CIA%TYPE,
                        p_cBodega IN INVE.ARINSE.BODEGA%TYPE,
                        p_cTipoDoc IN INVE.ARINSE.TIPO_DOC%TYPE,
                        p_nSecuencia IN INVE.ARINSE.SECUENCIA%TYPE,
                        p_cError IN OUT VARCHAR2
                         );
  
  -- PROCEDIMIENTO QUE NOS PERMITE GUARDAR LA GUIA FICTA
  PROCEDURE GUARDAR_GUIA_FICTA(p_cNoCia IN FACTU.ARPFOE.NO_CIA%TYPE,
                               p_cNoOrden IN FACTU.ARPFOE.NO_ORDEN%TYPE,
                               p_cNoGuia OUT FACTU.ARFAFE.NO_GUIA%TYPE,
                               p_nNoDocu OUT INVE.ARINSE.SECUENCIA%TYPE,                      
                               p_cError IN OUT VARCHAR2
                              );
  
  -- FACTURAR GUIA FICTA
  PROCEDURE FACTURAR_GUIA_FICTA(p_cNoCia IN FACTU.ARPFFE.NO_CIA%TYPE,
                                p_cBodega IN FACTU.ARPFFE.BODEGA%TYPE,
                                p_cNoGuia IN FACTU.ARPFFE.NO_GUIA%TYPE,
                                p_cTipoDoc IN FACTU.ARPFFE.TIPO_DOC%TYPE,
                                p_cNoFactu IN FACTU.ARPFFE.NO_FACTU%TYPE,
                                p_cError IN OUT VARCHAR2
                               );

END PR_GUIA;
/