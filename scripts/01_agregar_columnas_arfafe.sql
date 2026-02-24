-- ══════════════════════════════════════════════════════════════════════════════
-- SCRIPT SQL PARA ORACLE - AGREGAR COLUMNAS FALTANTES EN FACTU.ARFAFE
-- ══════════════════════════════════════════════════════════════════════════════
-- 
-- Este script agrega las columnas que existen en factura2023.cabecera (MySQL)
-- pero que NO existen en FACTU.ARFAFE (Oracle)
--
-- Ejecutar como usuario FACTU o con privilegios sobre el esquema
-- ══════════════════════════════════════════════════════════════════════════════

SET SERVEROUTPUT ON;

-- ══════════════════════════════════════════════════════════════════════════════
-- 1. COLUMNA ID_EXTERNO - Identificador externo SUNAT
-- Equivale a: idExterno VARCHAR(45) en MySQL
-- Formato: RUC-TIPO-SERIE-CORRELATIVO (ej: 20548167150-01-F001-0000097)
-- ══════════════════════════════════════════════════════════════════════════════
DECLARE
    v_existe NUMBER;
BEGIN
    SELECT COUNT(*) INTO v_existe 
    FROM USER_TAB_COLUMNS 
    WHERE TABLE_NAME = 'ARFAFE' AND COLUMN_NAME = 'ID_EXTERNO';
    
    IF v_existe = 0 THEN
        EXECUTE IMMEDIATE 'ALTER TABLE FACTU.ARFAFE ADD ID_EXTERNO VARCHAR2(50)';
        DBMS_OUTPUT.PUT_LINE('✅ Columna ID_EXTERNO agregada');
    ELSE
        DBMS_OUTPUT.PUT_LINE('⚠️ Columna ID_EXTERNO ya existe');
    END IF;
END;
/

COMMENT ON COLUMN FACTU.ARFAFE.ID_EXTERNO IS 'ID externo SUNAT: RUC-TIPO-SERIE-CORRELATIVO';

-- ══════════════════════════════════════════════════════════════════════════════
-- 2. COLUMNA DOCU_ENVIAWS - Flag para enviar a SUNAT
-- Equivale a: docu_enviaws VARCHAR(45) DEFAULT 'S' en MySQL
-- ══════════════════════════════════════════════════════════════════════════════
DECLARE
    v_existe NUMBER;
BEGIN
    SELECT COUNT(*) INTO v_existe 
    FROM USER_TAB_COLUMNS 
    WHERE TABLE_NAME = 'ARFAFE' AND COLUMN_NAME = 'DOCU_ENVIAWS';
    
    IF v_existe = 0 THEN
        EXECUTE IMMEDIATE 'ALTER TABLE FACTU.ARFAFE ADD DOCU_ENVIAWS VARCHAR2(1) DEFAULT ''S''';
        DBMS_OUTPUT.PUT_LINE('✅ Columna DOCU_ENVIAWS agregada');
    ELSE
        DBMS_OUTPUT.PUT_LINE('⚠️ Columna DOCU_ENVIAWS ya existe');
    END IF;
END;
/

COMMENT ON COLUMN FACTU.ARFAFE.DOCU_ENVIAWS IS 'Enviar a SUNAT: S=Si, N=No';

-- ══════════════════════════════════════════════════════════════════════════════
-- 3. COLUMNA CDR_OBSERVACION - Observaciones del CDR
-- Equivale a: cdr_observacion VARCHAR(2000) en MySQL
-- ══════════════════════════════════════════════════════════════════════════════
DECLARE
    v_existe NUMBER;
BEGIN
    SELECT COUNT(*) INTO v_existe 
    FROM USER_TAB_COLUMNS 
    WHERE TABLE_NAME = 'ARFAFE' AND COLUMN_NAME = 'CDR_OBSERVACION';
    
    IF v_existe = 0 THEN
        EXECUTE IMMEDIATE 'ALTER TABLE FACTU.ARFAFE ADD CDR_OBSERVACION VARCHAR2(2000)';
        DBMS_OUTPUT.PUT_LINE('✅ Columna CDR_OBSERVACION agregada');
    ELSE
        DBMS_OUTPUT.PUT_LINE('⚠️ Columna CDR_OBSERVACION ya existe');
    END IF;
END;
/

COMMENT ON COLUMN FACTU.ARFAFE.CDR_OBSERVACION IS 'Observaciones adicionales del CDR de SUNAT';

-- ══════════════════════════════════════════════════════════════════════════════
-- 4. COLUMNA DOCU_LINK_PDF - Ruta al archivo PDF
-- Equivale a: docu_link_pdf VARCHAR(200) en MySQL
-- ══════════════════════════════════════════════════════════════════════════════
DECLARE
    v_existe NUMBER;
BEGIN
    SELECT COUNT(*) INTO v_existe 
    FROM USER_TAB_COLUMNS 
    WHERE TABLE_NAME = 'ARFAFE' AND COLUMN_NAME = 'DOCU_LINK_PDF';
    
    IF v_existe = 0 THEN
        EXECUTE IMMEDIATE 'ALTER TABLE FACTU.ARFAFE ADD DOCU_LINK_PDF VARCHAR2(300)';
        DBMS_OUTPUT.PUT_LINE('✅ Columna DOCU_LINK_PDF agregada');
    ELSE
        DBMS_OUTPUT.PUT_LINE('⚠️ Columna DOCU_LINK_PDF ya existe');
    END IF;
END;
/

COMMENT ON COLUMN FACTU.ARFAFE.DOCU_LINK_PDF IS 'Ruta completa al archivo PDF del comprobante';

-- ══════════════════════════════════════════════════════════════════════════════
-- 5. COLUMNA DOCU_LINK_CDR - Ruta al archivo CDR
-- Equivale a: docu_link_cdr VARCHAR(200) en MySQL
-- ══════════════════════════════════════════════════════════════════════════════
DECLARE
    v_existe NUMBER;
BEGIN
    SELECT COUNT(*) INTO v_existe 
    FROM USER_TAB_COLUMNS 
    WHERE TABLE_NAME = 'ARFAFE' AND COLUMN_NAME = 'DOCU_LINK_CDR';
    
    IF v_existe = 0 THEN
        EXECUTE IMMEDIATE 'ALTER TABLE FACTU.ARFAFE ADD DOCU_LINK_CDR VARCHAR2(300)';
        DBMS_OUTPUT.PUT_LINE('✅ Columna DOCU_LINK_CDR agregada');
    ELSE
        DBMS_OUTPUT.PUT_LINE('⚠️ Columna DOCU_LINK_CDR ya existe');
    END IF;
END;
/

COMMENT ON COLUMN FACTU.ARFAFE.DOCU_LINK_CDR IS 'Ruta completa al archivo CDR de SUNAT';

-- ══════════════════════════════════════════════════════════════════════════════
-- 6. COLUMNA DOCU_LINK_XML - Ruta al archivo XML
-- Equivale a: docu_link_xml VARCHAR(200) en MySQL
-- Nota: Ya existe NOMBRE_RQ pero solo guarda el nombre, no la ruta completa
-- ══════════════════════════════════════════════════════════════════════════════
DECLARE
    v_existe NUMBER;
BEGIN
    SELECT COUNT(*) INTO v_existe 
    FROM USER_TAB_COLUMNS 
    WHERE TABLE_NAME = 'ARFAFE' AND COLUMN_NAME = 'DOCU_LINK_XML';
    
    IF v_existe = 0 THEN
        EXECUTE IMMEDIATE 'ALTER TABLE FACTU.ARFAFE ADD DOCU_LINK_XML VARCHAR2(300)';
        DBMS_OUTPUT.PUT_LINE('✅ Columna DOCU_LINK_XML agregada');
    ELSE
        DBMS_OUTPUT.PUT_LINE('⚠️ Columna DOCU_LINK_XML ya existe');
    END IF;
END;
/

COMMENT ON COLUMN FACTU.ARFAFE.DOCU_LINK_XML IS 'Ruta completa al archivo XML firmado';

-- ══════════════════════════════════════════════════════════════════════════════
-- 7. ÍNDICES PARA MEJORAR RENDIMIENTO
-- ══════════════════════════════════════════════════════════════════════════════

-- Índice para buscar documentos pendientes de envío
DECLARE
    v_existe NUMBER;
BEGIN
    SELECT COUNT(*) INTO v_existe 
    FROM USER_INDEXES 
    WHERE INDEX_NAME = 'IDX_ARFAFE_PENDIENTES_FE';
    
    IF v_existe = 0 THEN
        EXECUTE IMMEDIATE 'CREATE INDEX IDX_ARFAFE_PENDIENTES_FE ON FACTU.ARFAFE (NO_CIA, ESTADO, TIPO_DOC, FECHA)';
        DBMS_OUTPUT.PUT_LINE('✅ Índice IDX_ARFAFE_PENDIENTES_FE creado');
    ELSE
        DBMS_OUTPUT.PUT_LINE('⚠️ Índice IDX_ARFAFE_PENDIENTES_FE ya existe');
    END IF;
END;
/

-- Índice para buscar por ID externo
DECLARE
    v_existe NUMBER;
BEGIN
    SELECT COUNT(*) INTO v_existe 
    FROM USER_INDEXES 
    WHERE INDEX_NAME = 'IDX_ARFAFE_ID_EXTERNO';
    
    IF v_existe = 0 THEN
        EXECUTE IMMEDIATE 'CREATE INDEX IDX_ARFAFE_ID_EXTERNO ON FACTU.ARFAFE (ID_EXTERNO)';
        DBMS_OUTPUT.PUT_LINE('✅ Índice IDX_ARFAFE_ID_EXTERNO creado');
    ELSE
        DBMS_OUTPUT.PUT_LINE('⚠️ Índice IDX_ARFAFE_ID_EXTERNO ya existe');
    END IF;
END;
/

-- ══════════════════════════════════════════════════════════════════════════════
-- 8. TABLA LOG_ENVIO_SUNAT (OPCIONAL - Para registro de envíos)
-- ══════════════════════════════════════════════════════════════════════════════
DECLARE
    v_existe NUMBER;
BEGIN
    SELECT COUNT(*) INTO v_existe 
    FROM USER_TABLES 
    WHERE TABLE_NAME = 'LOG_ENVIO_SUNAT';
    
    IF v_existe = 0 THEN
        EXECUTE IMMEDIATE '
        CREATE TABLE FACTU.LOG_ENVIO_SUNAT (
            ID_LOG          NUMBER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
            NO_CIA          VARCHAR2(2) NOT NULL,
            TIPO_DOC        VARCHAR2(2) NOT NULL,
            NO_FACTU        VARCHAR2(11) NOT NULL,
            FECHA_ENVIO     DATE DEFAULT SYSDATE,
            AMBIENTE        VARCHAR2(20),
            RESULTADO       VARCHAR2(10),
            CODIGO_RESP     VARCHAR2(20),
            MENSAJE_RESP    VARCHAR2(2000),
            HASHCODE        VARCHAR2(100),
            TIEMPO_RESP_MS  NUMBER,
            USUARIO         VARCHAR2(50) DEFAULT USER
        )';
        DBMS_OUTPUT.PUT_LINE('✅ Tabla LOG_ENVIO_SUNAT creada');
        
        EXECUTE IMMEDIATE 'CREATE INDEX IDX_LOG_SUNAT_FECHA ON FACTU.LOG_ENVIO_SUNAT (NO_CIA, FECHA_ENVIO DESC)';
        EXECUTE IMMEDIATE 'CREATE INDEX IDX_LOG_SUNAT_FACTU ON FACTU.LOG_ENVIO_SUNAT (NO_CIA, NO_FACTU)';
    ELSE
        DBMS_OUTPUT.PUT_LINE('⚠️ Tabla LOG_ENVIO_SUNAT ya existe');
    END IF;
END;
/

COMMENT ON TABLE FACTU.LOG_ENVIO_SUNAT IS 'Log de envíos de comprobantes electrónicos a SUNAT';

-- ══════════════════════════════════════════════════════════════════════════════
-- 9. VISTA PARA DOCUMENTOS PENDIENTES DE ENVÍO
-- ══════════════════════════════════════════════════════════════════════════════
CREATE OR REPLACE VIEW FACTU.VW_DOCS_PENDIENTES_FE AS
SELECT 
    A.NO_CIA,
    A.TIPO_DOC,
    CASE A.TIPO_DOC 
        WHEN 'F' THEN '01 - FACTURA'
        WHEN 'B' THEN '03 - BOLETA'
        WHEN 'C' THEN '07 - NOTA CREDITO'
        WHEN 'D' THEN '08 - NOTA DEBITO'
    END AS DESC_TIPO_DOC,
    A.NO_FACTU,
    SUBSTR(A.NO_FACTU, 1, 4) || '-' || SUBSTR(A.NO_FACTU, 5) AS NUMERO_FORMATEADO,
    A.FECHA,
    A.NO_CLIENTE,
    A.NBR_CLIENTE,
    A.NUM_DOC_CLI,
    A.MONEDA,
    A.VALOR_VENTA,
    A.IMPUESTO,
    A.TOTAL,
    A.ESTADO,
    A.FEC_CREA
FROM FACTU.ARFAFE A
WHERE A.ESTADO = 'D'
AND A.TIPO_DOC IN ('F', 'B', 'C', 'D')
ORDER BY A.FECHA, A.NO_FACTU;

COMMENT ON TABLE FACTU.VW_DOCS_PENDIENTES_FE IS 'Vista de documentos pendientes de envío a SUNAT (ESTADO=D)';

-- ══════════════════════════════════════════════════════════════════════════════
-- 10. VISTA PARA DOCUMENTOS ENVIADOS A SUNAT
-- ══════════════════════════════════════════════════════════════════════════════
CREATE OR REPLACE VIEW FACTU.VW_DOCS_ENVIADOS_FE AS
SELECT 
    A.NO_CIA,
    A.TIPO_DOC,
    CASE A.TIPO_DOC 
        WHEN 'F' THEN '01 - FACTURA'
        WHEN 'B' THEN '03 - BOLETA'
        WHEN 'C' THEN '07 - NOTA CREDITO'
        WHEN 'D' THEN '08 - NOTA DEBITO'
    END AS DESC_TIPO_DOC,
    A.NO_FACTU,
    A.ID_EXTERNO,
    A.FECHA,
    A.NO_CLIENTE,
    A.NBR_CLIENTE,
    A.TOTAL,
    A.ESTADO,
    A.COD_HASH AS HASHCODE,
    A.ESTADO_SUNAT AS CDR,
    A.MENSAJE_ERROR_TCI AS CDR_NOTA,
    A.FEC_ENVIO,
    A.DOCU_LINK_PDF,
    A.DOCU_LINK_XML,
    A.DOCU_LINK_CDR
FROM FACTU.ARFAFE A
WHERE A.ESTADO = 'E'
AND A.TIPO_DOC IN ('F', 'B', 'C', 'D')
ORDER BY A.FEC_ENVIO DESC;

COMMENT ON TABLE FACTU.VW_DOCS_ENVIADOS_FE IS 'Vista de documentos enviados exitosamente a SUNAT (ESTADO=E)';

-- ══════════════════════════════════════════════════════════════════════════════
-- FIN DEL SCRIPT
-- ══════════════════════════════════════════════════════════════════════════════
COMMIT;

DBMS_OUTPUT.PUT_LINE('');
DBMS_OUTPUT.PUT_LINE('══════════════════════════════════════════════════════════════════');
DBMS_OUTPUT.PUT_LINE('                    SCRIPT EJECUTADO EXITOSAMENTE                  ');
DBMS_OUTPUT.PUT_LINE('══════════════════════════════════════════════════════════════════');
DBMS_OUTPUT.PUT_LINE('');
DBMS_OUTPUT.PUT_LINE('COLUMNAS AGREGADAS A FACTU.ARFAFE:');
DBMS_OUTPUT.PUT_LINE('  - ID_EXTERNO');
DBMS_OUTPUT.PUT_LINE('  - DOCU_ENVIAWS');
DBMS_OUTPUT.PUT_LINE('  - CDR_OBSERVACION');
DBMS_OUTPUT.PUT_LINE('  - DOCU_LINK_PDF');
DBMS_OUTPUT.PUT_LINE('  - DOCU_LINK_CDR');
DBMS_OUTPUT.PUT_LINE('  - DOCU_LINK_XML');
DBMS_OUTPUT.PUT_LINE('');
DBMS_OUTPUT.PUT_LINE('ÍNDICES CREADOS:');
DBMS_OUTPUT.PUT_LINE('  - IDX_ARFAFE_PENDIENTES_FE');
DBMS_OUTPUT.PUT_LINE('  - IDX_ARFAFE_ID_EXTERNO');
DBMS_OUTPUT.PUT_LINE('');
DBMS_OUTPUT.PUT_LINE('OBJETOS CREADOS:');
DBMS_OUTPUT.PUT_LINE('  - Tabla LOG_ENVIO_SUNAT');
DBMS_OUTPUT.PUT_LINE('  - Vista VW_DOCS_PENDIENTES_FE');
DBMS_OUTPUT.PUT_LINE('  - Vista VW_DOCS_ENVIADOS_FE');
DBMS_OUTPUT.PUT_LINE('══════════════════════════════════════════════════════════════════');
