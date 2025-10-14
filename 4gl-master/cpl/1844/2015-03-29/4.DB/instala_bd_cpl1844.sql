CREATE TABLE ret_sol_issste_constancia(
nss         CHAR(11)     ,
consecutivo DECIMAL(11,0),
constancia  CHAR(01)
) IN ret_dbs1
;

CREATE INDEX ix_ret_sol_cons ON ret_sol_issste_constancia(nss,consecutivo);