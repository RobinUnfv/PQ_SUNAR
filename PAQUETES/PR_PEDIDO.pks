CREATE OR REPLACE PACKAGE FACTU.PR_PEDIDO IS
 --***************************************************************
  -- Author  : ROBINZON SANTANA LLACZA
  -- Created : 27/01/2024
  -- Purpose : MODULO FACTURACION - PEDIDO
  --Log de cambios
  -- Fecha          Autor    Codigo         Descripcion
  -- 17/01/2024   ROBINZON S.   <RTC313089>    CREACION 
  --***************************************************************
  
  -- Declaracion Typo Registro
  TYPE rMsgValidaRecordType IS RECORD
  (
    Proc    VARCHAR2(50),
    CodErr  VARCHAR2(20),
    MsgErr  VARCHAR2(150),
    FecPro  DATE,
    UsuCre  VARCHAR2(15)
  );
  rMsgValidaRec   rMsgValidaRecordType;
  
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
                             );
  
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
                        );
    -- PROCESO PARA MOSTRAR EL RESULTADO EN FORMATO XML COMO TEXTO                    
  PROCEDURE SetXMLOut(
            p_cNoCia IN FACTU.ARPFOE.NO_CIA%TYPE,
            p_cNoOrden IN FACTU.ARPFOE.NO_ORDEN%TYPE,
            p_cNoCliente IN FACTU.ARPFOE.NO_CLIENTE%TYPE,
            p_cError IN VARCHAR2,
            p_cXMLError IN XMLTYPE,
            --
            P_cXMLOut OUT NOCOPY CLOB
          );
          
  -- Concatenar los errores encontrados en formato XML
  PROCEDURE PUSH_ERROR(
       p_cProceso IN VARCHAR2,
       p_cCodError IN VARCHAR2,
       p_cMsg IN VARCHAR2,
       p_dFecProc IN DATE,
       p_cUser IN VARCHAR2,
       --
       p_cXMLError IN OUT NOCOPY XMLTYPE     
  );
   
  -- INSERTAR EN LA TABLA ARPFOE
  PROCEDURE GUARDAR_ARPFOE(UNO IN TDU_TABLA_ARPFOE, p_cError IN OUT VARCHAR2, p_cXMLError IN OUT NOCOPY XMLTYPE);
  
  -- FUNCIÓN QUE DEVUELVE EL TDU CORRESPONDIENTE AL ARPFOE
  FUNCTION CREAR_TDU_ARPFOE( p_cTag IN VARCHAR2, p_cXmlInput IN CLOB) RETURN FACTU.TDU_TABLA_ARPFOE;
  
  -- PROCEDIMIENTO QUE REALIZA VALIDACIONES PRELIMINARES COMUNES
  PROCEDURE PRE_VALIDACION( p_tdu_arpfoe FACTU.TDU_TABLA_ARPFOE, p_cError IN OUT VARCHAR2, p_cXMLError IN OUT NOCOPY XMLTYPE );
  
  -- VALIDAR NUMERO DE PEDIDO
  PROCEDURE VALIDA_NUM_PEDIDO(p_cNoCia IN VARCHAR2, 
                               p_cCentro IN VARCHAR2, 
                               p_cNoOrder IN VARCHAR2,
                               p_cError IN OUT VARCHAR2, 
                               p_cXMLError IN OUT NOCOPY XMLTYPE);
  
  -- PROCEDIMIENTO PARA CARGAR DATOS DEL DETALLE DEL PEDIDO
  PROCEDURE CARGAR_TDU_TABLA_ARPFOL( p_cTag IN VARCHAR2, 
                                     p_cXmlInput IN CLOB,
                                     p_tdu_arpfol IN OUT NOCOPY FACTU.TDU_TABLA_ARPFOL,
                                     p_cError IN OUT VARCHAR2,
                                     p_cXMLError IN OUT NOCOPY XMLTYPE);
                                     
  -- PROCEDIMIENTO PARA GUARDAR EN LA TABLA FACTU.ARPFOL
  PROCEDURE GUARDAR_ARPFOL(DET IN FACTU.TDU_TABLA_ARPFOL, p_cError IN OUT VARCHAR2, p_cXMLError IN OUT NOCOPY XMLTYPE);
    
  -- PROCESO PARA EMITIR PEDIDO
  PROCEDURE EMISION_PEDIDO(p_cXmlInput IN CLOB, p_cXmlOutput IN OUT NOCOPY CLOB);
  
  -- PROCEDIMIENTO QUE VA ACTUALIZAR O INSERTAR UN PEDIDO
  PROCEDURE GUARDAR_PEDIDO(p_cXmlInput IN CLOB, 
                           p_cNoOrder OUT FACTU.ARPFOE.NO_ORDEN%TYPE,
                           p_cError OUT VARCHAR2);
                           
    -- PROCESO QUE NOS PERMITE OBTENER EL ARRAY DE ARPFOE
  PROCEDURE SET_TDU_ARPFOE(p_cTag IN VARCHAR2,
                      p_cXmlInput IN CLOB,
                      p_cError IN OUT VARCHAR2,
                      p_tArpfoe OUT FACTU.TDU_TABLA_ARPFOE,
                      p_cNoOrder OUT FACTU.ARPFOE.NO_ORDEN%TYPE
                      );
                      
  -- PROCESO QUE NOS PERMITE OBTENER EL ARRAY DE ARPFOL
  PROCEDURE SET_TDU_ARPFOL(p_cTag IN VARCHAR2,
                      p_cXmlInput IN CLOB,
                      p_cError IN OUT VARCHAR2,
                      p_tArpfol OUT FACTU.TDU_TABLA_ARPFOL,
                      p_cNoOrder IN FACTU.ARPFOE.NO_ORDEN%TYPE);                          
   
  
END PR_PEDIDO;
/