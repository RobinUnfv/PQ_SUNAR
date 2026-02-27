-- ============================================================
-- 1. CREAR TABLA T_RESUMEN_DIARIO
-- ============================================================
CREATE TABLE FACTU.T_RESUMEN_DIARIO (
    NO_CIA VARCHAR2(2) NOT NULL,
    FEC_EMISION DATE NOT NULL,
    --FEC_RESDIA DATE DEFAULT SYSDATE,
    NRO_CORRELATIVO VARCHAR2(5) NOT NULL,
	--RUC_EMISOR VARCHAR2(14),
    ESTADO VARCHAR2(2) DEFAULT 'N',
    TICKET VARCHAR2(50),
    CDR VARCHAR2(200),
	DESCRIPCION VARCHAR2(500),
    CONSTRAINT PK_RESDIA PRIMARY KEY (NO_CIA, FEC_EMISION, NRO_CORRELATIVO)
);

-- ============================================================
-- 2. COMENTARIOS
-- ============================================================
COMMENT ON TABLE FACTU.T_RESUMEN_DIARIO IS 'Registro de resumen diario enviadas a SUNAT';
COMMENT ON COLUMN FACTU.T_RESUMEN_DIARIO.NO_CIA IS 'Código de compañía';
--COMMENT ON COLUMN FACTU.T_RESUMEN_DIARIO.FEC_EMISION IS 'Fecha de emisión del comprobante de pago';
COMMENT ON COLUMN FACTU.T_RESUMEN_DIARIO.FEC_RESDIA IS 'Fecha de creación del resumen diario';
COMMENT ON COLUMN FACTU.T_RESUMEN_DIARIO.NRO_CORRELATIVO IS 'Número correlativo';
--COMMENT ON COLUMN FACTU.T_RESUMEN_DIARIO.RUC_EMISOR IS 'RUC del Emision';
COMMENT ON COLUMN FACTU.T_RESUMEN_DIARIO.ESTADO IS 'N = Nuevo, B = Bloqueo, P = Proceso, E = Enviado, X = Error de Envio';
COMMENT ON COLUMN FACTU.T_RESUMEN_DIARIO.TICKET IS 'Número de TICKET';
COMMENT ON COLUMN FACTU.T_RESUMEN_DIARIO.CDR IS 'Constancia de Recepción devuelta por SUNAT';
COMMENT ON COLUMN FACTU.T_RESUMEN_DIARIO.DESCRIPCION IS 'Descripción de respuesta de SUNAT';

-- ============================================================
-- 3. ÍNDICES
-- ============================================================
CREATE INDEX IDX_CIA_FECEMI ON FACTU.T_RESUMEN_DIARIO(NO_CIA, FEC_EMISION);

-- ============================================================
-- 4. GRANTS (Ajustar según necesidad)
-- ============================================================
GRANT SELECT, INSERT, UPDATE ON FACTU.T_RESUMEN_DIARIO TO YPC;
GRANT SELECT, INSERT, UPDATE ON FACTU.T_RESUMEN_DIARIO TO FACTU;

-- ============================================================
-- FIN DEL SCRIPT
-- ============================================================
