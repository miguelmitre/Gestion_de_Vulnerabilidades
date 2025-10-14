CREATE FUNCTION fn_obten_fecha_val_des( p_fecha_genera DATE )
RETURNING DATE ;

DEFINE v_fecha_val      DATE;
DEFINE v_fecha_sig      DATE;
DEFINE v_mes_sig        SMALLINT;
DEFINE v_ano_sig        SMALLINT;

--CPL-1199 Se crea funcion especial para el proceso de desinversion a peticion de la afore.

LET v_mes_sig = MONTH(p_fecha_genera);
LET v_ano_sig = YEAR (p_fecha_genera);

LET v_fecha_val = MDY ( v_mes_sig, 1, v_ano_sig );

RETURN v_fecha_val ;

END FUNCTION;


