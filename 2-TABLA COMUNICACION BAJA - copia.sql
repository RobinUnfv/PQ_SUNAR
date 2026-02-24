-- ============================================================
-- 1. CREAR TABLA COMUNICACION_BAJA
-- ============================================================
CREATE TABLE FACTU.COMUNICACION_BAJA (
    NO_CIA VARCHAR2(10) NOT NULL,
    NO_FACTU VARCHAR2(11) NOT NULL,
    FEC_EMISION DATE NOT NULL,
    FEC_BAJA DATE DEFAULT TRUNC(SYSDATE) NOT NULL,
    COD_MOTIVO VARCHAR2(2) NOT NULL,
    DESC_MOTIVO VARCHAR2(500),
    ESTADO VARCHAR2(2) DEFAULT 'N',
    NRO_CORRELATIVO VARCHAR2(5),
    TICKET_SUNAT VARCHAR2(50),
    CDR_SUNAT VARCHAR2(200),
    CONSTRAINT PK_BAJA PRIMARY KEY (NO_CIA, NO_FACTU)
);

-- ============================================================
-- 2. COMENTARIOS
-- ============================================================
COMMENT ON TABLE FACTU.COMUNICACION_BAJA IS 'Registro de comunicaciones de baja enviadas a SUNAT';
COMMENT ON COLUMN FACTU.COMUNICACION_BAJA.NO_CIA IS 'Código de compañía';
COMMENT ON COLUMN FACTU.COMUNICACION_BAJA.NO_FACTU IS 'Número completo del comprobante (ej: F001-00000123)';
COMMENT ON COLUMN FACTU.COMUNICACION_BAJA.FEC_EMISION IS 'Fecha de emisión del comprobante';
COMMENT ON COLUMN FACTU.COMUNICACION_BAJA.FEC_BAJA IS 'Fecha de la baja';
COMMENT ON COLUMN FACTU.COMUNICACION_BAJA.COD_MOTIVO IS 'Código del motivo según catálogo SUNAT (01-08)';
COMMENT ON COLUMN FACTU.COMUNICACION_BAJA.DESC_MOTIVO IS 'Descripción adicional del motivo';
COMMENT ON COLUMN FACTU.COMUNICACION_BAJA.ESTADO IS 'N = Nuevo, B = Bloqueo, P = Proceso, E = Enviado, X = Error de Envio';
COMMENT ON COLUMN FACTU.COMUNICACION_BAJA.NRO_CORRELATIVO IS 'Número de comunicación generado (RC-YYYYMMDD-###)';
COMMENT ON COLUMN FACTU.COMUNICACION_BAJA.TICKET_SUNAT IS 'Número de ticket devuelto por SUNAT';
COMMENT ON COLUMN FACTU.COMUNICACION_BAJA.CDR_SUNAT IS 'Constancia de Recepción devuelta por SUNAT';

-- ============================================================
-- 3. ÍNDICES
-- ============================================================
CREATE INDEX IDX_BAJA_FEC_EMISION ON FACTU.COMUNICACION_BAJA(NO_CIA, FEC_EMISION);

-- ============================================================
-- 4. GRANTS (Ajustar según necesidad)
-- ============================================================
GRANT SELECT, INSERT, UPDATE ON FACTU.COMUNICACION_BAJA TO YPC;
GRANT SELECT, INSERT, UPDATE ON FACTU.COMUNICACION_BAJA TO FACTU;

-- ============================================================
-- FIN DEL SCRIPT
-- ============================================================
