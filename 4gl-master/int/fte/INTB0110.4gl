#*******************************************************************#
#Proyecto          => Sistema de Afores.( MEXICO )                  #
#Propietario       => E.F.P.                                        #
#Programa          => INTB0110    Y INTB01005                       #
#Descripcion       => NOTIFICACION DE CONSTANCIA DE UNIFICACION DE  #
#                  => CTAS.EN ADMINISTRADORA                        #
#Sistema           => INT .                                         #
#Fecha             => 14 de mayo del 2002    .                      #
#Por               => Laura Eugenia Cortes Guzman                   #
#Modificado por    => Miguel Angel Hernandez Martinez               #
#Fecha modi        => 23 de abril del 2007   .                      #
#*******************************************************************#
DATABASE safre_af
GLOBALS
    "INTB01005.4gl"
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
          nss_uni            CHAR(11),
          ejecuta            CHAR(200),
          f_arch             CHAR(06),
          fliquida           DATE,
          farch              DATE

   OPTIONS INPUT WRAP,
           PROMPT LINE LAST,
           ACCEPT KEY CONTROL-I

##   DEFER INTERRUPT       
   CALL STARTLOG ("INTB0110.log")

    INITIALIZE fliquida, n_seguro, v_det, v_cza, v_ruta, ejecuta, fecha TO NULL
    INITIALIZE hora, farch, p_unifica.* TO NULL

    LET ban               = 0
    LET numero_reg        = 0
    LET consecutivo_envio = 0
    LET hoy_hoy           = TODAY
    LET hora              = TIME
    LET hora1 = hora[1,2],hora[4,5],hora[7,8]

    SELECT c.ruta_envio,
           USER
    INTO   v_ruta ,
           usuario
    FROM   seg_modulo c
    WHERE  c.modulo_cod = "int"

    LET hoy = TODAY USING "mmddyy"
    LET hora1 = hora[1,2],hora[4,5],hora[7,8]

    SELECT v.nom_arch,
           arch_det,
           arch_cza 
    INTO   v_arch, v_det, v_cza
    FROM   tab_layout v
    WHERE  v.layout_cod = 410

    OPEN WINDOW INTB0110 AT 2,2 WITH FORM "INTB01101" ATTRIBUTE(BORDER)
         DISPLAY " < ESC > Procesar                                          < Ctrl-C > Salir    " AT 1,1 ATTRIBUTE(REVERSE)
                                                                                
         DISPLAY " INTB0110          CONSTANCIA DE UNIFICACION DE CUENTAS                        "
                 AT 3,1 ATTRIBUTE(REVERSE)
         DISPLAY hoy_hoy  USING "DD-MM-YYYY " AT 3,66 ATTRIBUTE(REVERSE)

         CONSTRUCT inter_noti ON   fliquida, nss_uni
                              FROM fliquida, nss_uni

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
            LET selec_tab_1 = " SELECT a.nss_uni, a.nss_cta1, a.cve_ent_cta1, ",
                              " a.fliquida, a.folio_liquida  ",
                              " FROM  uni_unificado a ",
                              " WHERE a.estado = 100 ",
                              " AND   a.", inter_noti CLIPPED

             LET farch    = inter_noti[11,20]
             IF farch IS NULL OR farch = " " THEN
                LET f_arch = TODAY USING "mmddyy"
             ELSE
                LET f_arch = farch USING "mmddyy"
             END IF
{
             LET v_arch = "30222",
                          f_arch CLIPPED,hora1 CLIPPED,
                          ".",
                          v_arch
             LET v_det  = v_det  CLIPPED
             LET v_cza  = v_cza  CLIPPED

}
             LET v_arch = v_ruta CLIPPED,"/", "30222",
                          f_arch CLIPPED,hora1 CLIPPED,
                          ".",
                          v_arch
             LET v_det  = v_ruta CLIPPED,"/", v_det  CLIPPED
             LET v_cza  = v_ruta CLIPPED,"/", v_cza  CLIPPED

             DISPLAY "GENERANDO INFORMACION.... AGUARDE UN MOMENTO..." at 19,1
                     ATTRIBUTE(REVERSE)

             LET leyenda_cza = "CONSTANCIA DE UNIFICACION DE CUENTAS EN ADMI",
                               "NISTRADORA DE FONDOS PARA EL RETIRO"

             START REPORT r_report TO v_det
             CALL detalle_notifica_uni("22",410,2)

             START REPORT r_report TO v_cza
             CALL cabeza_notifica_uni("22",leyenda_cza,410,1)

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
    CLOSE WINDOW INTB0110 
END MAIN
