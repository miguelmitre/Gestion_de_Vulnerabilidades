DROP TABLE tab_urv_rp;

CREATE TABLE tab_urv_rp
(
edad           SMALLINT,
ini_vigencia   DATE,
fin_vigencia   DATE,
tasa           DECIMAL(5,4),
urv_hombre     DECIMAL(24,18),
urv_mujer      DECIMAL(24,18),
PRIMARY KEY (edad,ini_vigencia)
);