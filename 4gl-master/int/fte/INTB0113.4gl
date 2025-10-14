#*******************************************************************#
#Proyecto          => Sistema de Afores.( MEXICO )                  #
#Propietario       => E.F.P.                                        #
#Programa          => INTB0113 Y INTB0100                           #
#Descripcion       => NOTIFICACION DE ACLARACION DE FONDOS PARA EL  #
#                  => RETIRO                                        #
#Sistema           => INT .                                         #
#Fecha             => 11 de abril del 2002   .                      #
#Por               => Laura Eugenia Cortes Guzman                   #
#*******************************************************************#
DATABASE safre_af
GLOBALS
    "INTB0100.4gl"
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
          farch              DATE,
          f_arch             CHAR(06),
          ejecuta            CHAR(200)

   OPTIONS INPUT WRAP,
           PROMPT LINE LAST,
           ACCEPT KEY CONTROL-I

##   DEFER INTERRUPT       
   CALL STARTLOG ("INTB0113.log")

    LET ban               = 0
    LET numero_reg        = 0
    LET consecutivo_envio = 0
    LET hoy_hoy = TODAY
    LET hora    = TIME

    INITIALIZE fentcons, n_seguro, v_det, v_cza, v_ruta, ejecuta, fecha TO NULL
    INITIALIZE farch, f_arch TO NULL

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
        WHERE v.layout_cod = 413

    OPEN WINDOW INTB0113 AT 2,2 WITH FORM "INTB01131" ATTRIBUTE(BORDER)
         DISPLAY " < ESC > Procesar                                          < ",
                 "Ctrl-C > Salir    " AT 1,1 ATTRIBUTE(REVERSE)
                                                                                
         DISPLAY " INTB0113             ACLARACION DE REGISTRO                                   "
                 AT 3,1 ATTRIBUTE(REVERSE)
         DISPLAY hoy_hoy  USING "DD-MM-YYYY " AT 3,66 ATTRIBUTE(REVERSE)

         CONSTRUCT inter_noti ON   fentcons, n_seguro
                              FROM fentcons, n_seguro

                   ON KEY(ESC)
                      LET ban = 0
                      EXIT CONSTRUCT

                   ON KEY(INTERRUPT)
                         LET ban = 1
                         EXIT CONSTRUCT

                   ON KEY(CONTROL-C)
                         LET ban = 1
                         EXIT CONSTRUCT
         END CONSTRUCT 


         IF ban  = 0 THEN
            LET selec_tab = " SELECT b.nss,   a.n_rfc, a.n_unico,  ",
                            " a.paterno,    a.materno,  a.nombres, ",
                            " a.sexo, a.n_folio, a.fentcons, a.tipo_solicitud ",
                            " FROM  afi_solicitud a, int_ctr_carta b",
                            " WHERE a.status_interno = 55 ",
                            " AND   a.tipo_solicitud = 1  ",
                            " AND   a.n_seguro = b.nss    ",
                            " AND   a.fentcons = b.fecha_registro ",
                            " AND   b.docto_cod = 30204   ",
                            " AND   b.edo_genera = 10     ",
                            " AND ", inter_noti CLIPPED

             LET farch    = inter_noti[11,20]
             IF farch IS NULL OR farch = " " THEN
                 LET f_arch = TODAY USING "mmddyy"
             ELSE
                 LET f_arch = farch USING "mmddyy"
             END IF
{
             LET v_arch = "30204",
                          f_arch CLIPPED,hora1 CLIPPED,".",v_arch
             LET v_det  = v_det  CLIPPED
             LET v_cza  = v_cza  CLIPPED
}
             LET v_arch = v_ruta CLIPPED,"/", "30204",
                          f_arch CLIPPED,hora1 CLIPPED,".",v_arch
             LET v_det  = v_ruta CLIPPED,"/", v_det  CLIPPED
             LET v_cza  = v_ruta CLIPPED,"/", v_cza  CLIPPED


             DISPLAY "GENERANDO INFORMACION.... AGUARDE UN MOMENTO..." at 19,1
                     ATTRIBUTE(REVERSE)

             LET leyenda_cza = "ACLARACION DE REGISTRO EN ADMINISTRADORA DE ",
                               "FONDOS PARA EL RETIRO"

             START REPORT r_report TO v_det
             CALL detalle_notifica("04",413,2)

             START REPORT r_report TO v_cza
             CALL cabeza_notifica("04",leyenda_cza,413,1)

             LET ejecuta = "cat ",v_cza," ",v_det," > ",v_arch
             RUN ejecuta

             LET ejecuta = "rm ",v_cza," ",v_det
             RUN ejecuta

             DISPLAY "" AT 19,1
             DISPLAY "ARCHIVO GENERADO EN : ",v_arch AT 19,1
             PROMPT "PROCESO FINALIZADO...< ENTER > PARA CONTINUAR " FOR enter
         ELSE
            PROMPT "PROCESO CANCELADO...< ENTER > PARA CONTINUAR " FOR enter
         END IF
    CLOSE WINDOW INTB0113 
END MAIN
