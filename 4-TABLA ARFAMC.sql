-- ============================================================
-- 1. CREAR TABLA FACTU.SUCURSAL_PTOVTA
-- ============================================================
grant select, insert, update, delete, references on FACTU.SUCURSAL_PTOVTA to PUBLIC with grant option;

ALTER TABLE FACTU.ARFAMC
MODIFY NOMBRE_ANO VARCHAR2(300);

