# ══════════════════════════════════════════════════════════════════════════════
# ANÁLISIS DETALLADO: Columnas Faltantes entre MySQL y Oracle
# ══════════════════════════════════════════════════════════════════════════════

## 1. CABECERA: factura2023.cabecera vs FACTU.ARFAFE

### 1.1 Columnas de MySQL (cabecera) que NO EXISTEN en Oracle (ARFAFE)

| # | Campo MySQL              | Tipo MySQL      | ¿Existe en ARFAFE? | Solución                              |
|---|--------------------------|-----------------|--------------------|-----------------------------------------|
| 1 | `idExterno`              | VARCHAR(45)     | ❌ **NO EXISTE**   | **AGREGAR** columna ID_EXTERNO          |
| 2 | `empr_razonsocial`       | VARCHAR(200)    | ❌ No en ARFAFE    | Consultar tabla **FACTU.ARFAMC.NOMBRE** |
| 3 | `empr_ubigeo`            | VARCHAR(20)     | ❌ No en ARFAFE    | Consultar **CXC.ARCCTDA** (CODI_DEPA+CODI_PROV+CODI_DIST) |
| 4 | `empr_nombrecomercial`   | VARCHAR(200)    | ❌ No en ARFAFE    | Consultar **FACTU.ARFAMC.NOMBRE**       |
| 5 | `empr_direccion`         | VARCHAR(450)    | ❌ No en ARFAFE    | Consultar **CXC.ARCCTDA.DIRECCION**     |
| 6 | `empr_provincia`         | VARCHAR(45)     | ❌ No en ARFAFE    | Consultar **CXC.ARCCPR.DESC_PROV**      |
| 7 | `empr_departamento`      | VARCHAR(45)     | ❌ No en ARFAFE    | Consultar **CXC.ARCCDP.DESC_DEPA**      |
| 8 | `empr_distrito`          | VARCHAR(45)     | ❌ No en ARFAFE    | Consultar **CXC.ARCCDI.DESC_DIST**      |
| 9 | `empr_pais`              | VARCHAR(10)     | ❌ No en ARFAFE    | Valor fijo **'PE'**                     |
| 10| `empr_nroruc`            | VARCHAR(45)     | ❌ No en ARFAFE    | Consultar **FACTU.ARFAMC.NO_CLIENTE_ONLINE** |
| 11| `empr_tipodoc`           | VARCHAR(45)     | ❌ No en ARFAFE    | Valor fijo **'6'** (RUC)                |
| 12| `docu_hora`              | TIME            | ⚠️ Parcial         | Extraer de **FEC_CREA** con TO_CHAR     |
| 13| `tasa_isc`               | VARCHAR(45)     | ❌ **NO EXISTE**   | Calcular o usar valor fijo **0**        |
| 14| `docu_otrostributos`     | VARCHAR(45)     | ❌ **NO EXISTE**   | Valor fijo **0**                        |
| 15| `tasa_otrostributos`     | VARCHAR(45)     | ❌ **NO EXISTE**   | Valor fijo **0**                        |
| 16| `docu_percepcion`        | VARCHAR(45)     | ❌ **NO EXISTE**   | Valor fijo **0**                        |
| 17| `cdr_observacion`        | VARCHAR(2000)   | ❌ **NO EXISTE**   | **AGREGAR** columna CDR_OBSERVACION     |
| 18| `docu_enviaws`           | VARCHAR(45)     | ❌ **NO EXISTE**   | **AGREGAR** columna DOCU_ENVIAWS        |
| 19| `docu_link_pdf`          | VARCHAR(200)    | ❌ **NO EXISTE**   | **AGREGAR** columna DOCU_LINK_PDF       |
| 20| `docu_link_cdr`          | VARCHAR(200)    | ❌ **NO EXISTE**   | **AGREGAR** columna DOCU_LINK_CDR       |
| 21| `docu_link_xml`          | VARCHAR(200)    | ⚠️ Parcial         | Existe **NOMBRE_RQ** pero solo nombre   |
| 22| `clie_correo_cpe1`       | VARCHAR(100)    | ❌ No en ARFAFE    | Consultar **CXC.ARCCMC.EMAIL**          |
| 23| `clie_correo_cpe2`       | VARCHAR(100)    | ❌ **NO EXISTE**   | Opcional - no necesario                 |

### 1.2 Columnas de MySQL que SÍ EXISTEN en Oracle (con diferente nombre)

| Campo MySQL              | Campo Oracle ARFAFE      | Conversión Necesaria                    |
|--------------------------|--------------------------|------------------------------------------|
| `docu_codigo`            | NO_CIA+TIPO_DOC+NO_FACTU | PK compuesta - usar hashCode()           |
| `clie_numero`            | NUM_DOC_CLI / NO_CLIENTE | Ya existe                                |
| `clie_tipodoc`           | TIPO_DOC_CLI             | Convertir: DNI-1, RUC-6                  |
| `clie_nombre`            | NBR_CLIENTE              | Ya existe                                |
| `docu_fecha`             | FECHA                    | Ya existe (DATE)                         |
| `docu_tipodocumento`     | TIPO_DOC                 | Convertir: F-01, B-03, C-07, D-08        |
| `docu_numero`            | NO_FACTU                 | Formatear: F0010000097 - F001-0000097    |
| `docu_moneda`            | MONEDA                   | Convertir: SOL-PEN, DOL-USD              |
| `docu_gravada`           | OPER_GRAVADAS            | Ya existe (NUMBER)                       |
| `docu_inafecta`          | OPER_INAFECTAS           | Ya existe (NUMBER)                       |
| `docu_exonerada`         | OPER_EXONERADAS          | Ya existe (NUMBER)                       |
| `docu_gratuita`          | OPER_GRATUITAS           | Ya existe (NUMBER)                       |
| `docu_descuento`         | T_DESCUENTO              | Ya existe (NUMBER)                       |
| `docu_subtotal`          | VALOR_VENTA              | Ya existe (NUMBER)                       |
| `docu_total`             | TOTAL                    | Ya existe (NUMBER)                       |
| `docu_igv`               | IMPUESTO                 | Ya existe (NUMBER)                       |
| `tasa_igv`               | IGV                      | Ya existe (default 18)                   |
| `docu_isc`               | IMP_ISC                  | Ya existe (NUMBER)                       |
| `docu_otroscargos`       | GASTOS                   | Ya existe (NUMBER)                       |
| `hashcode`               | COD_HASH                 | Ya existe                                |
| `cdr`                    | ESTADO_SUNAT             | Ya existe (parcial)                      |
| `cdr_nota`               | MENSAJE_ERROR_TCI        | Ya existe                                |
| `docu_proce_status`      | ESTADO                   | D=Pendiente, E=Enviado, X=Error          |
| `docu_proce_fecha`       | FEC_ENVIO                | Ya existe (DATE)                         |
| `docu_tipodocumento_anular` | TIPO_REFE_FACTU       | Para NC/ND - Ya existe                   |
| `docu_tipodocumento_numero` | NO_REFE_FACTU          | Para NC/ND - Ya existe                   |
| `docu_motivoanular`      | MOTIVO_NC                | Ya existe                                |

---

## 2. DETALLE: factura2023.detalle vs FACTU.ARFAFL

### 2.1 Columnas de MySQL (detalle) - ANÁLISIS COMPLETO

| # | Campo MySQL               | Tipo MySQL      | Campo ARFAFL        | Estado              |
|---|---------------------------|-----------------|---------------------|---------------------|
| 1 | `iddetalle`               | INT (PK)        | CONSECUTIVO         | ✅ Ya existe        |
| 2 | `docu_codigo`             | INT (FK)        | NO_CIA+TIPO_DOC+NO_FACTU | ✅ FK compuesta |
| 3 | `item_orden`              | VARCHAR(45)     | CONSECUTIVO         | ✅ Ya existe        |
| 4 | `item_unidad`             | VARCHAR(45)     | MEDIDA              | ✅ Ya existe        |
| 5 | `item_cantidad`           | VARCHAR(45)     | CANTIDAD_FACT       | ✅ Ya existe        |
| 6 | `item_codproducto`        | VARCHAR(45)     | NO_ARTI             | ✅ Ya existe        |
| 7 | `item_descripcion`        | VARCHAR(255)    | DESCRIPCION         | ✅ Ya existe        |
| 8 | `item_afectacion`         | VARCHAR(45)     | TIPO_AFECTACION     | ✅ Ya existe        |
| 9 | `item_tipo_precio_venta`  | VARCHAR(45)     | -                   | ❌ Valor fijo '01'  |
| 10| `item_pventa`             | VARCHAR(45)     | PRECIO_UNIT         | ✅ Ya existe        |
| 11| `item_pventa_nohonerosa`  | VARCHAR(45)     | -                   | ❌ Calcular         |
| 12| `item_to_subtotal`        | VARCHAR(45)     | TOTAL               | ✅ Ya existe        |
| 13| `item_to_igv`             | VARCHAR(45)     | IMP_IGV             | ✅ Ya existe        |
| 14| `item_pvtaigv`            | VARCHAR(45)     | PREC_IGV            | ✅ Ya existe        |

### 2.2 Conclusión DETALLE
✅ **La tabla ARFAFL tiene TODOS los campos necesarios** - No necesitas agregar columnas.

---

## 3. SCRIPT SQL PARA AGREGAR CAMPOS EN ORACLE

```sql
-- ══════════════════════════════════════════════════════════════════════════════
-- CAMPOS A AGREGAR EN FACTU.ARFAFE PARA COMPATIBILIDAD CON SysCoreFacturacion
-- Ejecutar como usuario FACTU o con privilegios sobre el esquema
-- ══════════════════════════════════════════════════════════════════════════════

-- 1. Identificador externo SUNAT (RUC-TIPO-SERIE-CORRELATIVO)
-- ALTER TABLE FACTU.ARFAFE ADD ID_EXTERNO VARCHAR2(50);
-- COMMENT ON COLUMN FACTU.ARFAFE.ID_EXTERNO IS 'ID externo SUNAT: RUC-TIPO-SERIE-CORRELATIVO';

-- 1. Estado del CDR
ALTER TABLE FACTU.ARFAFE ADD CDR VARCHAR2(200);
COMMENT ON COLUMN FACTU.ARFAFE.CDR IS 'Estado del CDR';

-- 2. Nota del CDR
ALTER TABLE FACTU.ARFAFE ADD CDR_NOTA VARCHAR2(500);
COMMENT ON COLUMN FACTU.ARFAFE.CDR_NOTA IS 'Nota del CDR';

-- 3. Observaciones del CDR
ALTER TABLE FACTU.ARFAFE ADD CDR_OBSERVACION VARCHAR2(2000);
COMMENT ON COLUMN FACTU.ARFAFE.CDR_OBSERVACION IS 'Observaciones adicionales del CDR';

-- 4. Flag para indicar si se debe enviar a SUNAT
ALTER TABLE FACTU.ARFAFE ADD ENVIAWS VARCHAR2(2) DEFAULT 'S';
COMMENT ON COLUMN FACTU.ARFAFE.ENVIAWS IS 'Enviar a SUNAT: S = Si, N = No';

-- 5. Estado de envio a SUNAT
ALTER TABLE FACTU.ARFAFE ADD PROCE_STATUS VARCHAR2(4) DEFAULT 'N';
COMMENT ON COLUMN FACTU.ARFAFE.PROCE_STATUS IS 'N = Nuevo, B = Bloqueo, P = Proceso, E = Enviado, X = Error de Envio';

-- 6. Fecha de envio del documento a SUNAT
ALTER TABLE FACTU.ARFAFE ADD PROCE_FECHA DATE;
COMMENT ON COLUMN FACTU.ARFAFE.PROCE_FECHA IS 'Fecha de envio del documento a SUNAT';

-- 7. Tipo documento anular
ALTER TABLE FACTU.ARFAFE ADD TIP_DOC_ANULAR VARCHAR2(10);
COMMENT ON COLUMN FACTU.ARFAFE.TIP_DOC_ANULAR IS 'Tipo documento anular';

-- 8. Numero de documento anular
ALTER TABLE FACTU.ARFAFE ADD NUM_ANULAR VARCHAR2(45);
COMMENT ON COLUMN FACTU.ARFAFE.NUM_ANULAR IS 'Numero de documento anular';

-- 9. Motivo de anular
ALTER TABLE FACTU.ARFAFE ADD MOT_ANULAR VARCHAR2(200);
COMMENT ON COLUMN FACTU.ARFAFE.MOT_ANULAR IS 'Motivo de anular';

-- ══════════════════════════════════════════════════════════════════════════════
-- ÍNDICES PARA MEJORAR RENDIMIENTO
-- ══════════════════════════════════════════════════════════════════════════════

CREATE INDEX IDX_ARFAFE_PENDIENTES ON FACTU.ARFAFE (NO_CIA, PROCE_STATUS, TIPO_DOC, FECHA);
```

---

## 4. TABLAS ORACLE A CONSULTAR (basado en PR_FACTURA.pkb)

| Tabla                    | Esquema | Datos que contiene                              |
|--------------------------|---------|--------------------------------------------------|
| `ARFAMC`                 | FACTU   | Empresa: NOMBRE, NO_CLIENTE_ONLINE (RUC)         |
| `ARCCMC`                 | CXC     | Clientes: EMAIL, TIPO_DOCUMENTO, NU_DOCUMENTO    |
| `ARCCTDA`                | CXC     | Direcciones: DIRECCION, CODI_DEPA/PROV/DIST      |
| `ARCCDP`                 | CXC     | Departamentos: DESC_DEPA                         |
| `ARCCPR`                 | CXC     | Provincias: DESC_PROV                            |
| `ARCCDI`                 | CXC     | Distritos: DESC_DIST                             |
| `ARINDA`                 | INVE    | Artículos: DESCRIPCION                           |
| `ARFCRED`                | FACTU   | Cuotas crédito: NO_CREDITO, MONTO, FEC_PAGO      |
| `TABLAS_SUNAT_FE`        | CONTA   | Catálogos SUNAT                                  |
