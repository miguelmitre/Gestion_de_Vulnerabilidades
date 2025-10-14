DROP TABLE pen_matriz_anio_semanas;
CREATE TABLE pen_matriz_anio_semanas
  (
    anio        SMALLINT,
    semanas_min SMALLINT,
    semanas_max SMALLINT,
    columna     SMALLINT
  );

DROP TABLE pen_matriz_sem_cotiza;
CREATE TABLE pen_matriz_sem_cotiza
  (
    edad               SMALLINT,
    salario_min_inf    DECIMAL(12,2),
    salario_min_sup    DECIMAL(12,2),
    pesos_pension      DECIMAL(12,2),
    columna            SMALLINT,
    fecha_ini_vigencia DATE,
    fecha_fin_vigencia DATE,
    usuario            CHAR(8)
  );

LOAD FROM pen_matriz_anio_semanas_CPL-3557.unl INSERT INTO pen_matriz_anio_semanas;
LOAD FROM pen_matriz_sem_cotiza_CPL-3557.unl INSERT INTO pen_matriz_sem_cotiza;
