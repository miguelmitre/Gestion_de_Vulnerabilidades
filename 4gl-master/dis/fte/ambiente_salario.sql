DROP PROCEDURE ambiente_salario;
CREATE PROCEDURE ambiente_salario(r_per_pago INTEGER);

   --################################   IMSS  ###############################

   IF EXISTS (SELECT "X" FROM SYSTABLES WHERE tabname = "dis_aporte") THEN
      DROP TABLE dis_aporte;
   END IF

   CREATE table dis_aporte
   (
    n_seguro           CHAR(11),
    periodo_pago       INTEGER,
    ult_salario_diario DECIMAL(12,2),
    folio              INTEGER,
    id_aportante       CHAR(13)
   )in dis_dbs1;

   INSERT INTO dis_aporte
   SELECT n_seguro,periodo_pago,ult_salario_diario/100, folio,reg_patronal_imss
   FROM   safre_af:dis_det_aporte;

   CREATE INDEX dis_aporte1 on dis_aporte(n_seguro);
   CREATE INDEX dis_aporte2 on dis_aporte(periodo_pago);

   UPDATE STATISTICS FOR TABLE dis_aporte;

   IF EXISTS (SELECT "X" FROM SYSTABLES WHERE tabname = "dis_aporte_uni") THEN
      DROP TABLE dis_aporte_uni;
   END IF

   CREATE TABLE dis_aporte_uni
   (
    n_seguro      CHAR(11),
    periodo_pago  INTEGER
   )in dis_dbs1;

   INSERT INTO dis_aporte_uni
   SELECT n_seguro, max(periodo_pago)
   FROM   dis_aporte
   WHERE  periodo_pago < r_per_pago
   GROUP BY 1;

   CREATE INDEX dis_aporuni1 on dis_aporte_uni(n_seguro,periodo_pago);

   UPDATE STATISTICS FOR TABLE dis_aporte_uni;

   IF EXISTS (SELECT "X" FROM SYSTABLES WHERE tabname = "dis_aporte_cta") THEN
      DROP TABLE dis_aporte_cta;
   END IF

   CREATE table dis_aporte_cta
   (
    n_seguro           CHAR(11),
    periodo_pago       INTEGER,
    ult_salario_diario DECIMAL(12,2),
    folio              INTEGER,
    id_aportante       CHAR(13)
   )in dis_dbs1;


   INSERT INTO dis_aporte_cta
   SELECT a.*
   FROM  dis_aporte a, dis_aporte_uni b
   WHERE a.n_seguro     = b.n_seguro
   AND   a.periodo_pago = b.periodo_pago;

   CREATE INDEX dis_apor_cta1 on dis_aporte_cta(n_seguro);

   UPDATE STATISTICS FOR TABLE dis_aporte_cta;

   --################################  ISSSTE  ###############################

   IF EXISTS (SELECT "X" FROM SYSTABLES WHERE tabname = "dis_aporte_iste") THEN
      DROP TABLE dis_aporte_iste;
   END IF

   CREATE table dis_aporte_iste
   (
    n_unico            CHAR(18),
    periodo_pago       INTEGER,
    ult_salario_diario DECIMAL(12,2),
    folio              INTEGER,
    id_aportante       CHAR(13)
   )in dis_dbs1;

   INSERT INTO dis_aporte_iste
   SELECT n_unico, periodo_pago, (sueldo_base_cot_rcv/100)/60, folio,
          n_rfc_entidad
   FROM   safre_af:dis_det_issste;

   CREATE INDEX dis_apo_iste1 on dis_aporte_iste(n_unico);
   CREATE INDEX dis_apo_iste2 on dis_aporte_iste(periodo_pago);

   UPDATE STATISTICS FOR TABLE dis_aporte_iste;

   IF EXISTS (SELECT "X" FROM SYSTABLES WHERE tabname = "dis_apo_iste_uni") THEN
      DROP TABLE dis_apo_iste_uni;
   END IF

   CREATE TABLE dis_apo_iste_uni
   (
    n_unico       CHAR(18),
    periodo_pago  INTEGER
   )in dis_dbs1;

   INSERT INTO dis_apo_iste_uni
   SELECT n_unico, max(periodo_pago)
   FROM   dis_aporte_iste
   WHERE  periodo_pago < r_per_pago
   GROUP BY 1;

   CREATE INDEX dis_apo_isuni1 on dis_apo_iste_uni(n_unico,periodo_pago);

   UPDATE STATISTICS FOR TABLE dis_apo_iste_uni;

   IF EXISTS (SELECT "X" FROM SYSTABLES WHERE tabname = "dis_apo_cta_iste") THEN
      DROP TABLE dis_apo_cta_iste;
   END IF

   CREATE table dis_apo_cta_iste
   (
    n_unico            CHAR(18),
    periodo_pago       INTEGER,
    ult_salario_diario DECIMAL(12,2),
    folio              INTEGER,
    id_aportante       CHAR(13)
   )in dis_dbs1;

   INSERT INTO dis_apo_cta_iste
   SELECT a.*
   FROM  dis_aporte_iste a, dis_apo_iste_uni b
   WHERE a.n_unico      = b.n_unico
   AND   a.periodo_pago = b.periodo_pago;

   CREATE INDEX dis_apo_icta1 on dis_apo_cta_iste(n_unico);
   UPDATE STATISTICS FOR TABLE dis_apo_cta_iste;



END PROCEDURE;


