   CREATE RAW TABLE tmp_dis_cuenta_acr
    ( 
     tipo_movimiento      SMALLINT,
     subcuenta            SMALLINT,
     siefore              SMALLINT,
     folio                INTEGER,
     consecutivo_lote     INTEGER,
     nss                  CHAR(11),
     curp                 CHAR(18),
     folio_sua            CHAR(6),
     fecha_pago           DATE,
     fecha_valor          DATE,
     fecha_conversion     DATE,
     monto_en_pesos       DECIMAL(22,6),
     monto_en_acciones    DECIMAL(22,6),
     precio_accion        DECIMAL(22,6),
     dias_cotizados       SMALLINT,
     sucursal             CHAR(10),
     id_aportante         CHAR(11),
     estado               SMALLINT,
     fecha_proceso        DATE,
     usuario              CHAR(8),
     fecha_archivo        DATE,
     etiqueta             SMALLINT
    );

CREATE TABLE cuota_acr
  ( nss                CHAR(11),
    curp               CHAR(18),
    tipo_solicitud     SMALLINT,
    finicta            DATE
    );
