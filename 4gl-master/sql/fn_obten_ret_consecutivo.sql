DROP FUNCTION fn_obten_ret_consecutivo()
;
CREATE FUNCTION "syscoppel".fn_obten_ret_consecutivo()

    RETURNING DECIMAL(11,0);

    -- Obtiene el ultimo consecutivo de la tabla ret_consecutivo y
    -- actualiza su valor

    DEFINE ld_consecutivo DECIMAL(11,0);

    SET LOCK MODE TO WAIT 10;
    LOCK TABLE ret_consecutivo IN EXCLUSIVE MODE ;

        SELECT MAX(consecutivo)
        INTO   ld_consecutivo
        FROM   ret_consecutivo ;

        IF ld_consecutivo IS NULL THEN
            LET ld_consecutivo = 0 ;
        END IF

        LET  ld_consecutivo = ld_consecutivo + 1 ;

        INSERT INTO ret_consecutivo
        VALUES (ld_consecutivo);

    UNLOCK TABLE ret_consecutivo ;
    SET LOCK MODE TO NOT WAIT ;

    RETURN ld_consecutivo;

END FUNCTION;


