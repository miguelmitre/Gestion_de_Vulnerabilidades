database safre_af
globals
    define 
        vnss char(11)
end globals

main
    declare cur_1 cursor for
    select unique(nss)
    from   int_det_voluntaria

    foreach cur_1 into vnss

        call valida_cta_saldo_vol(vnss)
    end foreach
end main

FUNCTION valida_cta_saldo_vol(v_nss)
#vcsv-------------------------------
    DEFINE #loc #char
       enter                  CHAR(01) ,
       v_nss                  CHAR(11)

    DEFINE #loc #decimal
       v_sldo_sub3_sief1      DECIMAL(16,6) ,
       v_sldo_sub3_sief3      DECIMAL(16,6) ,
       v_sldo_sub10_sief1     DECIMAL(16,6) ,
       v_sldo_sub10_sief3     DECIMAL(16,6) ,
       v2_sldo_sub3_sief1     DECIMAL(16,6) ,
       v2_sldo_sub3_sief3     DECIMAL(16,6) ,
       v2_sldo_sub10_sief1    DECIMAL(16,6) ,
       v2_sldo_sub10_sief3    DECIMAL(16,6)

    LET v_sldo_sub3_sief1   = 0
    LET v_sldo_sub3_sief3   = 0
    LET v_sldo_sub10_sief1  = 0
    LET v_sldo_sub10_sief3  = 0
    LET v2_sldo_sub3_sief1  = 0
    LET v2_sldo_sub3_sief3  = 0
    LET v2_sldo_sub10_sief1 = 0
    LET v2_sldo_sub10_sief3 = 0

    SELECT SUM(A.monto_en_acciones)
    INTO   v_sldo_sub3_sief1
    FROM   dis_cuenta A
    WHERE  A.nss       = v_nss
    AND    A.subcuenta = 3
    AND    A.siefore   = 1

    SELECT SUM(A.monto_en_acciones)
    INTO   v_sldo_sub3_sief3
    FROM   dis_cuenta A
    WHERE  A.nss       = v_nss
    AND    A.subcuenta = 3
    AND    A.siefore   = 3

    SELECT SUM(A.monto_en_acciones)
    INTO   v_sldo_sub10_sief1
    FROM   dis_cuenta A
    WHERE  A.nss       = v_nss
    AND    A.subcuenta = 10
    AND    A.siefore   = 1

    SELECT SUM(A.monto_en_acciones)
    INTO   v_sldo_sub10_sief3
    FROM   dis_cuenta A
    WHERE  A.nss       = v_nss
    AND    A.subcuenta = 10
    AND    A.siefore   = 3

    SELECT SUM(A.saldo_acciones)
    INTO   v2_sldo_sub3_sief1
    FROM   cta_saldo_vol A
    WHERE  A.nss = v_nss
    AND    A.subcuenta = 3
    AND    A.siefore   = 1

    IF v2_sldo_sub3_sief1 IS NULL
    OR v2_sldo_sub3_sief1 = " "
    OR v2_sldo_sub3_sief1 < 0  THEN
        LET v2_sldo_sub3_sief1 = 0
    END IF

    IF v_sldo_sub3_sief1 <> v2_sldo_sub3_sief1 THEN
        DISPLAY "NO CUADRA CTA_SALDO_VOL VS DIS_CUENTA (subcta. 3 y sief. 1) NSS:",v_nss
    END IF

    SELECT SUM(A.saldo_acciones)
    INTO   v2_sldo_sub3_sief3
    FROM   cta_saldo_vol A
    WHERE  A.nss = v_nss
    AND    A.subcuenta = 3
    AND    A.siefore   = 3

    IF v2_sldo_sub3_sief3 IS NULL
    OR v2_sldo_sub3_sief3 = " "
    OR v2_sldo_sub3_sief3 < 0  THEN
        LET v2_sldo_sub3_sief3 = 0
    END IF

    IF v_sldo_sub3_sief3 <> v2_sldo_sub3_sief3 THEN
        DISPLAY "NO CUADRA CTA_SALDO_VOL VS DIS_CUENTA (subcta. 3 y sief. 3) NSS:",v_nss
    END IF

    SELECT SUM(A.saldo_acciones)
    INTO   v2_sldo_sub10_sief1
    FROM   cta_saldo_vol A
    WHERE  A.nss = v_nss
    AND    A.subcuenta = 10
    AND    A.siefore   = 1

    IF v2_sldo_sub10_sief1 IS NULL
    OR v2_sldo_sub10_sief1 = " "
    OR v2_sldo_sub10_sief1 < 0  THEN
        LET v2_sldo_sub10_sief1 = 0
    END IF

    IF v_sldo_sub10_sief1 <> v2_sldo_sub10_sief1 THEN
        DISPLAY "NO CUADRA CTA_SALDO_VOL VS DIS_CUENTA (subcta.10 y sief. 1) NSS:",v_nss
    END IF

    SELECT SUM(A.saldo_acciones)
    INTO   v2_sldo_sub10_sief3
    FROM   cta_saldo_vol A
    WHERE  A.nss = v_nss
    AND    A.subcuenta = 10
    AND    A.siefore   = 3

    IF v2_sldo_sub10_sief3 IS NULL
    OR v2_sldo_sub10_sief3 = " "
    OR v2_sldo_sub10_sief3 < 0   THEN
        LET v2_sldo_sub10_sief3 = 0
    END IF

    IF v_sldo_sub10_sief3 <> v2_sldo_sub10_sief3 THEN
        DISPLAY "NO CUADRA CTA_SALDO_VOL VS DIS_CUENTA (subcta.10 y sief. 3) NSS:",v_nss
    END IF
END FUNCTION

