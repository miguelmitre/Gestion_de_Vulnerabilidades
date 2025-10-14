#*******************************************************************#
#Proyecto          => Sistema de Afores.( MEXICO )                  #
#Propietario       => E.F.P.                                        #
#Programa          => INTB0106 Y INTB01009                          #
#Descripcion       => NOTIFICACION DE CONSTANCIA DE REGISTRO AFILIA-#
#                  => CION.                                         #
#Sistema           => INT .                                         #
#Fecha             => 11 de abril del 2002   .                      #
#Por               => Laura Eugenia Cortes Guzman                   #
#CPL-1686          => FSR 18/08/2014 archivo paso por usuario       #
#*******************************************************************#
DATABASE safre_af
GLOBALS
    "INTB01009.4gl"
#
# ---------------------------------------------------------------------
# main 
# ---------------------------------------------------------------------
#
MAIN
   DEFINE v_ruta, 
          v_arch,
          v_det, v_cza       CHAR(70),
          hora               CHAR(08),
          hora1              CHAR(06),
          f_arch             CHAR(06),
          reci               CHAR(1),
          nss                CHAR(11),
          farch              DATE,
          fentcons1          DATE,
          ejecuta            CHAR(200)

   OPTIONS INPUT WRAP,
           PROMPT LINE LAST,
           ACCEPT KEY CONTROL-I

   DEFER INTERRUPT       
     CALL STARTLOG(FGL_GETENV("USER")||".INTB0106.log")         

    LET reci = ARG_VAL(1)
    LET nss  = ARG_VAL(2)

    INITIALIZE fentcons, n_seguro, v_det, v_cza, v_ruta, ejecuta, fecha TO NULL
    INITIALIZE gen_compara.*, hora,farch, f_arch TO NULL

    {CALL llena_comision_registro() RETURNING gen_compara.*,ban

    IF ban <> 0 THEN
       ERROR "ERROR: Faltan comisiones "
       SLEEP 3
       EXIT PROGRAM
    END IF
}
    LET ban               = 0
    LET numero_reg        = 0
    LET consecutivo_envio = 0
    LET hoy_hoy           = TODAY
    LET hora              = TIME
    LET hora1 = hora[1,2],hora[4,5],hora[7,8]

    SELECT f.* INTO p_tabafore.*
        FROM tab_afore_local f

    SELECT c.ruta_envio, USER INTO v_ruta , usuario
        FROM seg_modulo c
        WHERE c.modulo_cod = "int"

    LET hoy = TODAY USING "mmddyy"
    LET hora1 = hora[1,2],hora[4,5],hora[7,8]

    SELECT v.nom_arch, arch_det, arch_cza 
        INTO  v_arch, v_det, v_cza
        FROM  tab_layout v
        WHERE v.layout_cod = 406

    IF reci <> "S" THEN
       OPEN WINDOW INTB0106 AT 2,2 WITH FORM "INTB01062" ATTRIBUTE(BORDER)
         DISPLAY " < ESC > Procesar                                          <",
                 " Ctrl-C > Salir    " AT 1,1 ATTRIBUTE(REVERSE)
                                                                                
            DISPLAY " INTB0106                CONSTANCIA DE AFILIACIO",
                    "N                              " AT 3,1 ATTRIBUTE(REVERSE)
            DISPLAY hoy_hoy  USING "DD-MM-YYYY " AT 3,66 ATTRIBUTE(REVERSE)

            INPUT fentcons FROM FORMONLY.fentcons

                      AFTER FIELD fentcons
                            IF fentcons IS NULL OR fentcons = " "  THEN
                               ERROR "Digite correctamente la Fecha"
                               NEXT FIELD fentcons
                            END IF
                      ON KEY(ESC)
                            IF fentcons IS NULL OR fentcons = " "  THEN
                               ERROR "Digite correctamente la Fecha"
                               NEXT FIELD fentcons
                            END IF

                            LET ban = 0
                            EXIT INPUT

                      ON KEY(INTERRUPT, CONTROL-C)
                            LET ban = 1
                            EXIT INPUT
            END INPUT 
            LET fentcons1 = fentcons USING "mm/dd/yyyy"

            IF ban  = 0 THEN
{
                LET selec_tab = 
                         " SELECT a.n_seguro,   a.n_rfc, a.n_unico,   ",
                         " a.paterno, a.materno, a.nombres, a.sexo, a.n_folio,",
                         " a.fentcons, a.tipo_solicitud, a.fecha_elaboracion, ",
                         " a.fena, a.nacionalidad, a.finitmte, a.finicta,  ",
                         " a.fecha_1a_afil",
                         " FROM  afi_mae_afiliado a, int_ctr_carta b "

                LET selec_tab = selec_tab CLIPPED,
                         " WHERE a.fentcons = b.fecha_registro ",
                         "   AND a.n_seguro  = b.nss           ",
                         "   AND a.n_folio   = b.n_folio       ",
                         "   AND b.docto_cod = 30201           ",
                         "   AND a.fentcons = '",
			 fentcons1,"'",
                         "   AND b.edo_genera= 10              ",
                         "   AND a.finitmte IS NULL ",
                         "   AND a.n_seguro NOT IN  ",
			 "(SELECT d.nss ",
                         " FROM   taa_viv_recepcion d ",
                         " WHERE  d.fecha_mov_banxico = ",
			 "        a.fentcons) "
}
                LET selec_tab = 
                         " SELECT a.n_seguro,   a.n_rfc, a.n_unico,   ",
                         " a.paterno, a.materno, a.nombres, a.sexo, a.n_folio,",
                         " a.fentcons, a.tipo_solicitud, a.fecha_elaboracion, ",
                         " a.fena, a.nacionalidad, a.finitmte, a.finicta,  ",
                         " a.fecha_1a_afil",
                         " FROM  afi_mae_afiliado a, int_ctr_carta b "

                LET selec_tab = selec_tab CLIPPED,
                         " WHERE a.fentcons = b.fecha_registro ",
                         "   AND a.n_seguro  = b.nss           ",
                         "   AND a.n_folio   = b.n_folio       ",
                         "   AND a.tipo_solicitud = b.tipo_solicitud ",
                         "   AND b.docto_cod = 30201           ",
                         "   AND b.edo_genera = 10             ",
                         "   AND a.fentcons = '",
			 fentcons1,"'"

                LET farch    = fentcons
                IF farch IS NULL OR farch = " " THEN
                    LET f_arch = TODAY USING "mmddyy"
                ELSE
                    LET f_arch = farch USING "mmddyy"
                END IF


                LET v_arch = v_ruta CLIPPED,"/", "30201",
                             f_arch CLIPPED, hora1 CLIPPED,
                             ".",   v_arch

                LET v_det  = v_ruta CLIPPED,"/", v_det  CLIPPED, "_", usuario CLIPPED #CPL-1686
                LET v_cza  = v_ruta CLIPPED,"/", v_cza  CLIPPED, "_", usuario CLIPPED #CPL-1686
{
                LET v_arch = "30201",
                             f_arch CLIPPED, hora1 CLIPPED,
                             ".",   v_arch

                LET v_det  = v_det  CLIPPED
                LET v_cza  = v_cza  CLIPPED
}
                DISPLAY "GENERANDO INFORMACION.... AGUARDE UN MOMENTO..." 
                        at 19,1 ATTRIBUTE(REVERSE)

                LET leyenda_cza = "CONSTANCIA DE REGISTRO"
                START REPORT r_report TO v_det
                CALL detalle_notifica91("01",406,2)

                START REPORT r_report TO v_cza
                CALL cabeza_notifica9("01",leyenda_cza,406,1)

                LET ejecuta = "cat ",v_cza," ",v_det," > ",v_arch
                RUN ejecuta

                LET ejecuta = "rm ",v_cza," ",v_det
                RUN ejecuta

                DISPLAY "" AT 18,1
                DISPLAY "" AT 19,1
                DISPLAY "ARCHIVO GENERADO EN : ",v_arch AT 18,1
                PROMPT "PROCESO FINALIZADO...< ENTER > ",
                       "PARA CONTINUAR " FOR enter
            ELSE
               PROMPT "PROCESO CANCELADO...< ENTER > PARA CONTINUAR " FOR enter
            END IF
       CLOSE WINDOW INTB0106 
    ELSE
                LET selec_tab = 
                         " SELECT a.n_seguro,   a.n_rfc, a.n_unico,   ",
                         " a.paterno, a.materno, a.nombres, a.sexo, a.n_folio,",
                         " a.fentcons, a.tipo_solicitud, a.fecha_elaboracion, ",
                         " a.fena, a.nacionalidad, a.finitmte, a.finicta,  ",
                         " a.fecha_1a_afil",
                         " FROM  afi_mae_afiliado a, int_ctr_carta b "

                LET selec_tab = selec_tab CLIPPED,
                         " WHERE a.n_seguro =",nss,
                         "   AND a.finitmte IS NULL ",
                         "   AND a.n_seguro NOT IN  ",
			 "(SELECT d.nss           ",
                         " FROM   taa_viv_recepcion d ",
                         " WHERE  d.fecha_mov_banxico = ",
			 "        a.fentcons) "

                LET f_arch = TODAY USING "mmddyy"

                LET v_arch = v_ruta CLIPPED,"/", "30201",
                             f_arch CLIPPED, hora1 CLIPPED,
                             ".",   v_arch
                LET v_det  = v_ruta CLIPPED,"/", v_det  CLIPPED, "_", usuario CLIPPED #CPL-1686
                LET v_cza  = v_ruta CLIPPED,"/", v_cza  CLIPPED, "_", usuario CLIPPED #CPL-1686

                DISPLAY "GENERANDO INFORMACION.... AGUARDE UN MOMENTO..." 
                        at 19,1 ATTRIBUTE(REVERSE)

                LET leyenda_cza = "CONSTANCIA DE REGISTRO "

                START REPORT r_report TO v_det
                CALL detalle_notifica91("01",406,2)

                START REPORT r_report TO v_cza
                CALL cabeza_notifica9("01",leyenda_cza,406,1)

                LET ejecuta = "cat ",v_cza," ",v_det," > ",v_arch
                RUN ejecuta

                LET ejecuta = "rm ",v_cza," ",v_det
                RUN ejecuta

                DISPLAY "" AT 19,1
                DISPLAY "ARCHIVO GENERADO EN : ",v_arch AT 19,1
                PROMPT "PROCESO FINALIZADO...< ENTER > ",
                       "PARA CONTINUAR " FOR enter
    END IF
END MAIN
