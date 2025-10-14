#Proyecto          => SISTEMA DE safre_af( MEXICO )
#Owner             => E.F.P.
#Programa DISB001  => CARGA DE HISTORICO DE APORTACIONES
#Fecha             => 26 DE JULIO DE 1997
#By                => J. DAVID HERNANDEZ O.
#Actualizacion     => FRANCO E. ULLOA V.
#Sistema           => DIS 
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE
        w_codigo_afore         LIKE tab_afore_local.codigo_afore ,
        g_param RECORD LIKE dis_parametro.*
{
    DEFINE reg_hdep RECORD
        tipo_registro                CHAR(02) ,
        ident_servicio               CHAR(02) ,
        ident_pago                   CHAR(16) ,
        importe                      CHAR(15) ,
        fech_liquidacion             CHAR(08) ,
        impt_aport_acept             CHAR(15) ,
        impt_aport_dev               CHAR(15) ,
        estado                       CHAR(01) ,
        filler                       CHAR(222)
    END RECORD
}
    DEFINE reg_hdep2 RECORD
        tipo_registro                CHAR(02)      ,
        ident_servicio               CHAR(02)      ,
        ident_pago                   CHAR(16)      ,
        importe                      DECIMAL(15,2) ,
        fech_liquidacion             DATE          ,
        impt_aport_acept             CHAR(15)      ,
        impt_aport_dev               DECIMAL(15,2) ,
        estado                       CHAR(01)
    END RECORD

    DEFINE
        enter                        CHAR(01)  ,
        ejecuta_2                    CHAR(300) ,
        ejecuta_3                    CHAR(300) ,
        ejecuta_4                    CHAR(300) ,
        nom_arch_generado            CHAR(21)  ,
        HOY			     DATE      ,
        HORA			     CHAR(8)

    DEFINE 
        d15_impt_total_rcv           ,
        d15_impt_total_int_rcv       ,
        d15_impt_aport_pat           ,
        d15_impt_total_int_pat       ,
        d15_impt_total_guber         ,
        d15_impt_total_int_gub       ,
        d15_impt_total_xpagar        ,
        d15_impt_aport_acept         ,
        d15_impt_aport_dev           ,
        d13_impt_aport_acept         DECIMAL(15,2),
        ultimo_folio INTEGER,
        cla_sel CHAR(500),
        vfolio INTEGER,
        vcont  SMALLINT,
        salida CHAR(200),
        opc    CHAR(1)
END GLOBALS

MAIN
    DEFER INTERRUPT 

    OPTIONS PROMPT LINE LAST
 
   LET vfolio = ARG_VAL(1)

   LET  HOY  = TODAY
   LET  HORA = TIME

   CALL init() 
   WHILE TRUE
       LET enter = "S"
       IF enter MATCHES "[SsNn]" THEN
           IF enter MATCHES "[Ss]" THEN
               ERROR "Procesando Archivo de Dispersion"
               CALL proceso_principal()

  LET ejecuta_3 = "cd ",g_param.ruta_envio CLIPPED,
                  "/;chmod 777 cza det_03 hdep sum"


LET ejecuta_2 = "cat ",g_param.ruta_envio CLIPPED,"/cza ",
                       g_param.ruta_envio CLIPPED,"/det_03 ",
                       g_param.ruta_envio CLIPPED,"/hdep ",
                       g_param.ruta_envio CLIPPED,"/sum > ",
                       g_param.ruta_envio CLIPPED,"/",nom_arch_generado CLIPPED

LET ejecuta_4="cp ",g_param.ruta_envio CLIPPED,"/",nom_arch_generado CLIPPED,
                 " ",g_param.ruta_envio CLIPPED,"/DIS",hoy USING "yyyymmdd","@",
                  hora[1,5],".bak" 

               RUN ejecuta_3
               RUN ejecuta_2
               RUN ejecuta_4
           ELSE
               ERROR "PROCESO FORZADO A FINALIZAR" SLEEP 2
               EXIT PROGRAM
           END IF
           EXIT WHILE
       END IF
   END WHILE
END MAIN

FUNCTION proceso_principal()
#--------------------------
    DEFINE  l_char_f_liquida		CHAR(10),
            opc char(01)

    DECLARE cur_8 CURSOR FOR
    SELECT tipo_registro ,
           ident_servicio ,
           ident_pago ,
           importe ,
           fech_liquidacion ,
           impt_aport_acept ,
           impt_aport_dev ,
           estado
      FROM dis_dep_aporte
     WHERE estado = 1
       AND folio = vfolio

    LET l_char_f_liquida = ""
    
    LET salida = g_param.ruta_envio CLIPPED,"/hdep"

    START REPORT hdep2_listado TO salida 

        FOREACH  cur_8 INTO reg_hdep2.*

            LET l_char_f_liquida = reg_hdep2.fech_liquidacion
            LET l_char_f_liquida = l_char_f_liquida[7,10],
                                   l_char_f_liquida[1, 2],
                                   l_char_f_liquida[4, 5] CLIPPED
                            
            OUTPUT  TO REPORT hdep2_listado(reg_hdep2.tipo_registro    ,
                                            reg_hdep2.ident_servicio   ,
                                            reg_hdep2.ident_pago       ,
                                            reg_hdep2.importe          ,
                                            reg_hdep2.fech_liquidacion ,
                                            reg_hdep2.impt_aport_acept ,
                                            reg_hdep2.impt_aport_dev   ,
                                            l_char_f_liquida                
                                           )
        END FOREACH
    FINISH REPORT hdep2_listado
END FUNCTION

FUNCTION init()
#--------------
    DEFINE
	   vfecha	CHAR(08),
           comando      CHAR(250)
    SELECT codigo_afore
    INTO   w_codigo_afore
    FROM   tab_afore_local

    SELECT *
      INTO g_param.*
      FROM dis_parametro 

    CREATE TEMP TABLE filedisper        
    (reg CHAR(295))                                                            

-----    LET comando = "/imss/DISPER/ENVIO/cza"

    LET comando = g_param.ruta_envio CLIPPED,"/cza" CLIPPED

    LOAD FROM comando INSERT INTO filedisper
    IF STATUS = NOTFOUND THEN
       DISPLAY  "NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO"
       EXIT PROGRAM                                                         
    END IF                                                                  
   SELECT count(*)
     INTO vcont
     FROM dis_dep_aporte
    WHERE folio = vfolio
   SELECT reg[17,24] INTO vfecha FROM filedisper                                    
   IF vcont > 5 THEN
      LET nom_arch_generado = vfecha,".CONFAC" CLIPPED
   ELSE
      LET nom_arch_generado = vfecha,".CONFAF" CLIPPED
   END IF

   #LET nom_arch_generado = "PREFT.DP.A01",w_codigo_afore USING"&&&","CONFAF"
END FUNCTION

REPORT hdep2_listado(reg_hdep)
#hl--------------------------
    DEFINE reg_hdep RECORD
        tipo_registro                LIKE dis_dep_aporte.tipo_registro   ,
        ident_servicio               LIKE dis_dep_aporte.ident_servicio  ,
        ident_pago                   LIKE dis_dep_aporte.ident_pago      ,
        importe                      LIKE dis_dep_aporte.importe         ,
        fech_liquidacion             LIKE dis_dep_aporte.fech_liquidacion,
        impt_aport_acept             LIKE dis_dep_aporte.impt_aport_acept,
        impt_aport_dev               LIKE dis_dep_aporte.impt_aport_dev  ,
        char_liquidacion             CHAR(8)
    END RECORD

    DEFINE 
        c16_importe             CHAR(16),
        c15_importe             CHAR(15),
        c16_impt_aport_acept    CHAR(16),
        c15_impt_aport_acept    CHAR(15),
        c16_impt_aport_dev      CHAR(16),
        c15_impt_aport_dev      CHAR(15)

    OUTPUT
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0
        PAGE LENGTH 1

    FORMAT
                 
    ON EVERY ROW
            IF reg_hdep2.importe IS NULL THEN
                LET c15_importe = "000000000000000"  
            ELSE
                LET c16_importe = reg_hdep2.importe USING"&&&&&&&&&&&&&.&&"
                LET c15_importe = c16_importe[01,13] ,
                                  c16_importe[15,16]
            END IF

            IF reg_hdep2.impt_aport_acept IS NULL THEN
                LET c15_impt_aport_acept = "000000000000000"  
            ELSE
                LET c16_impt_aport_acept = reg_hdep2.impt_aport_acept
                                           USING"&&&&&&&&&&&&&.&&"
                LET c15_impt_aport_acept = c16_impt_aport_acept[01,13] ,
                                           c16_impt_aport_acept[15,16]
            END IF

            IF reg_hdep2.impt_aport_dev IS NULL THEN
                LET c15_impt_aport_dev   = "000000000000000"  
            ELSE
                LET c16_impt_aport_dev = reg_hdep2.impt_aport_dev 
                                         USING"&&&&&&&&&&&&&.&&"
                LET c15_impt_aport_dev = c16_impt_aport_dev[01,13] ,
                                         c16_impt_aport_dev[15,16]
            END IF
        PRINT
            COLUMN 01,reg_hdep.tipo_registro    ,
                      reg_hdep.ident_servicio   ,
                      reg_hdep.ident_pago       ,
                      c15_importe               ,
                      reg_hdep.char_liquidacion ,
                      c15_impt_aport_acept      ,
                      c15_impt_aport_dev        ,
                      222 spaces
           
            UPDATE dis_dep_aporte
            SET estado = 2 
            WHERE tipo_registro =reg_hdep.tipo_registro
             AND ident_servicio =reg_hdep.ident_servicio
             AND ident_pago =reg_hdep.ident_pago
             AND fech_liquidacion =reg_hdep.fech_liquidacion
             AND impt_aport_acept =reg_hdep.impt_aport_acept
             AND folio =vfolio 

{
LET cla_sel="UPDATE dis_dep_aporte ",
            "SET estado = 2 ",
            "WHERE tipo_registro =","'",reg_hdep.tipo_registro,"'",
            " AND ident_servicio =","'",reg_hdep.ident_servicio,"'",
            " AND ident_pago =","'",reg_hdep.ident_pago,"'",
            " AND fech_liquidacion =","'",reg_hdep.fech_liquidacion,"'",
            " AND impt_aport_acept =",reg_hdep.impt_aport_acept,
            " AND folio =",ultimo_folio CLIPPED
       PREPARE claexe FROM cla_sel
       EXECUTE claexe
            ##" AND estado=1
}

END REPORT
