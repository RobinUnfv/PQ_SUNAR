
-- ══════════════════════════════════════════════════════════════════════════════════════
-- 1. CREAR TABLA PRINCIPAL DE CONFIGURACIÓN
-- ══════════════════════════════════════════════════════════════════════════════════════

CREATE TABLE FACTU.CONFIG_SUNAT (
    NO_CIA              VARCHAR2(10)    NOT NULL,
    CODIGO              VARCHAR2(50)    NOT NULL,
    VALOR               VARCHAR2(500)   NOT NULL,
    DESCRIPCION         VARCHAR2(200),
    GRUPO               VARCHAR2(50),
    TIPO_DATO           VARCHAR2(20)    DEFAULT 'STRING',
    ACTIVO              VARCHAR2(1)     DEFAULT 'S',
    CONSTRAINT PK_CONFIG_SUNAT PRIMARY KEY (NO_CIA, CODIGO)
)
-- Comentarios de la tabla
COMMENT ON TABLE FACTU.CONFIG_SUNAT IS 'Configuración de parámetros para facturación electrónica SUNAT';
COMMENT ON COLUMN FACTU.CONFIG_SUNAT.NO_CIA IS 'Código de compañía';
COMMENT ON COLUMN FACTU.CONFIG_SUNAT.CODIGO IS 'Código único del parámetro';
COMMENT ON COLUMN FACTU.CONFIG_SUNAT.VALOR IS 'Valor del parámetro';
COMMENT ON COLUMN FACTU.CONFIG_SUNAT.DESCRIPCION IS 'Descripción del parámetro';
COMMENT ON COLUMN FACTU.CONFIG_SUNAT.GRUPO IS 'Grupo: RUTAS, CERTIFICADO, SUNAT_AMBIENTE, SUNAT_CREDENCIALES, EMPRESA, LIMITES, IMPUESTOS';
COMMENT ON COLUMN FACTU.CONFIG_SUNAT.TIPO_DATO IS 'Tipo de dato: STRING, NUMBER, BOOLEAN, PASSWORD';
COMMENT ON COLUMN FACTU.CONFIG_SUNAT.ACTIVO IS 'S=Activo, N=Inactivo';

-- ══════════════════════════════════════════════════════════════════════════════════════
-- 2. INSERTAR DATOS DE CONFIGURACIÓN INICIALES
-- ══════════════════════════════════════════════════════════════════════════════════════

-- ════════════════════════════════════════════════════════════════
-- GRUPO: RUTAS - Directorios del sistema de archivos
-- ════════════════════════════════════════════════════════════════
-- Encontrado en: BoletaElectronica.java:64, FacturaElectronica.java:63, 
--                DarBajaDocElectronica.java:51, ResBolElectronica.java:71

INSERT INTO FACTU.CONFIG_SUNAT (NO_CIA, CODIGO, VALOR, DESCRIPCION, GRUPO, TIPO_DATO)
VALUES ('01', 'RUTA_ENVIO', 'd:\POS-SUNAT\envio\', 
        'Directorio para archivos XML/ZIP de envío a SUNAT', 'RUTAS', 'STRING');

-- Encontrado en: BoletaElectronica.java:137, FacturaElectronica.java:141,
--                DarBajaDocElectronica.java:52, ResBolElectronica.java:72
INSERT INTO FACTU.CONFIG_SUNAT (NO_CIA, CODIGO, VALOR, DESCRIPCION, GRUPO, TIPO_DATO)
VALUES ('01', 'RUTA_RESPUESTA', 'd:\POS-SUNAT\respuesta\', 
        'Directorio para archivos CDR de respuesta de SUNAT', 'RUTAS', 'STRING');

-- Encontrado en: BoletaElectronica.java:201, FacturaElectronica.java:204,
--                DarBajaDocElectronica.java:56, ResBolElectronica.java:76
INSERT INTO FACTU.CONFIG_SUNAT (NO_CIA, CODIGO, VALOR, DESCRIPCION, GRUPO, TIPO_DATO)
VALUES ('01', 'RUTA_CERTIFICADO', 'd:\POS-SUNAT\certificado.jks', 
        'Ruta completa del archivo de certificado digital (.jks)', 'RUTAS', 'STRING');

-- ════════════════════════════════════════════════════════════════
-- GRUPO: CERTIFICADO - Configuración del certificado digital
-- ════════════════════════════════════════════════════════════════
-- Encontrado en: BoletaElectronica.java:200, FacturaElectronica.java:203,
--                DarBajaDocElectronica.java:55, ResBolElectronica.java:75

INSERT INTO FACTU.CONFIG_SUNAT (NO_CIA, CODIGO, VALOR, DESCRIPCION, GRUPO, TIPO_DATO)
VALUES ('01', 'KEYSTORE_TYPE', 'JKS', 
        'Tipo de keystore (JKS o PKCS12)', 'CERTIFICADO', 'STRING');

-- Encontrado en: BoletaElectronica.java:202, FacturaElectronica.java:205,
--                DarBajaDocElectronica.java:57, ResBolElectronica.java:77
INSERT INTO FACTU.CONFIG_SUNAT (NO_CIA, CODIGO, VALOR, DESCRIPCION, GRUPO, TIPO_DATO)
VALUES ('01', 'KEYSTORE_PASS', '123456789', 
        'Contraseña del keystore', 'CERTIFICADO', 'PASSWORD');

-- Encontrado en: BoletaElectronica.java:203, FacturaElectronica.java:206,
--                DarBajaDocElectronica.java:58, ResBolElectronica.java:78
INSERT INTO FACTU.CONFIG_SUNAT (NO_CIA, CODIGO, VALOR, DESCRIPCION, GRUPO, TIPO_DATO)
VALUES ('01', 'PRIVATE_KEY_PASS', 'CORPTEx2218', 
        'Contraseña de la clave privada del certificado', 'CERTIFICADO', 'PASSWORD');

-- ════════════════════════════════════════════════════════════════
-- GRUPO: SUNAT_AMBIENTE - Configuración del ambiente SUNAT
-- ════════════════════════════════════════════════════════════════
-- Encontrado en: BoletaElectronica.java:96, FacturaElectronica.java:100,
--                DarBajaDocElectronica.java:61, ResBolElectronica.java:81

INSERT INTO FACTU.CONFIG_SUNAT (NO_CIA, CODIGO, VALOR, DESCRIPCION, GRUPO, TIPO_DATO)
VALUES ('01', 'AMBIENTE_SUNAT', '1', 
        'Ambiente SUNAT: 1=Beta, 2=Homologación/QA, 3=Producción', 'SUNAT_AMBIENTE', 'NUMBER');

INSERT INTO FACTU.CONFIG_SUNAT (NO_CIA, CODIGO, VALOR, DESCRIPCION, GRUPO, TIPO_DATO)
VALUES ('01', 'ENVIAR_SUNAT', 'S', 
        'Enviar documentos a SUNAT: S=Sí, N=No (solo genera XML)', 'SUNAT_AMBIENTE', 'BOOLEAN');

-- ════════════════════════════════════════════════════════════════
-- GRUPO: SUNAT_CREDENCIALES_BETA - Credenciales ambiente Beta/Demo
-- ════════════════════════════════════════════════════════════════
-- Encontrado en: HeaderHandler.java:57-62

INSERT INTO FACTU.CONFIG_SUNAT (NO_CIA, CODIGO, VALOR, DESCRIPCION, GRUPO, TIPO_DATO)
VALUES ('01', 'SUNAT_BETA_USUARIO', '20609272016MODDATOS', 
        'Usuario SOL ambiente Beta (RUC + Usuario secundario)', 'SUNAT_CREDENCIALES_BETA', 'STRING');

INSERT INTO FACTU.CONFIG_SUNAT (NO_CIA, CODIGO, VALOR, DESCRIPCION, GRUPO, TIPO_DATO)
VALUES ('01', 'SUNAT_BETA_CLAVE', 'MODDATOS', 
        'Clave SOL ambiente Beta', 'SUNAT_CREDENCIALES_BETA', 'PASSWORD');

INSERT INTO FACTU.CONFIG_SUNAT (NO_CIA, CODIGO, VALOR, DESCRIPCION, GRUPO, TIPO_DATO)
VALUES ('01', 'SUNAT_BETA_URL', 'https://e-beta.sunat.gob.pe/ol-ti-itcpfegem-beta/billService', 
        'URL del servicio web SUNAT Beta', 'SUNAT_CREDENCIALES_BETA', 'STRING');

-- ════════════════════════════════════════════════════════════════
-- GRUPO: SUNAT_CREDENCIALES_PROD - Credenciales ambiente Producción
-- ════════════════════════════════════════════════════════════════
-- Encontrado en: HeaderHandler.java:56,61 (comentados)

INSERT INTO FACTU.CONFIG_SUNAT (NO_CIA, CODIGO, VALOR, DESCRIPCION, GRUPO, TIPO_DATO)
VALUES ('01', 'SUNAT_PROD_USUARIO', '20609272016CORPOTCE', 
        'Usuario SOL ambiente Producción (RUC + Usuario secundario)', 'SUNAT_CREDENCIALES_PROD', 'STRING');

INSERT INTO FACTU.CONFIG_SUNAT (NO_CIA, CODIGO, VALOR, DESCRIPCION, GRUPO, TIPO_DATO)
VALUES ('01', 'SUNAT_PROD_CLAVE', 'DRAVErFACEL2', 
        'Clave SOL ambiente Producción', 'SUNAT_CREDENCIALES_PROD', 'PASSWORD');

INSERT INTO FACTU.CONFIG_SUNAT (NO_CIA, CODIGO, VALOR, DESCRIPCION, GRUPO, TIPO_DATO)
VALUES ('01', 'SUNAT_PROD_URL', 'https://e-factura.sunat.gob.pe/ol-ti-itcpfegem/billService', 
        'URL del servicio web SUNAT Producción', 'SUNAT_CREDENCIALES_PROD', 'STRING');

-- ════════════════════════════════════════════════════════════════
-- GRUPO: LIMITES - Límites y parámetros del sistema
-- ════════════════════════════════════════════════════════════════
-- Encontrado en: ResBolElectronica.java:84

INSERT INTO FACTU.CONFIG_SUNAT (NO_CIA, CODIGO, VALOR, DESCRIPCION, GRUPO, TIPO_DATO)
VALUES ('01', 'MAX_BOLETAS_RESUMEN', '100', 
        'Máximo de boletas por resumen diario (límite SUNAT)', 'LIMITES', 'NUMBER');

INSERT INTO FACTU.CONFIG_SUNAT (NO_CIA, CODIGO, VALOR, DESCRIPCION, GRUPO, TIPO_DATO)
VALUES ('01', 'MAX_REINTENTOS', '3', 
        'Máximo de reintentos en caso de error de conexión', 'LIMITES', 'NUMBER');

INSERT INTO FACTU.CONFIG_SUNAT (NO_CIA, CODIGO, VALOR, DESCRIPCION, GRUPO, TIPO_DATO)
VALUES ('01', 'TIMEOUT_CONEXION', '60000', 
        'Timeout de conexión en milisegundos (60 segundos)', 'LIMITES', 'NUMBER');
