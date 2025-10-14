#*******************************************************************#
#Proyecto          => Sistema de Afores.( MEXICO )                  #
#Propietario       => E.F.P.                                        #
#Programa          => INTB0153 Y INTB01003                          #
#Descripcion       => NOTIFICACION DE RECHAZO DE REGISTRO NO        #
#                  => AFILIADO 30533-447                            #
#Sistema           => INT .                                         #
#Fecha Elaboracion => 02 de Febrero del 2006 .                      #
#Elaborado por     => Laura Eugenia Cortes Guzman                   #
#Fecha ult.Modif.  => 02 de Febrero del 2006 .                      #
#Modificado por    => Laura Eugenia Cortes Guzman                   #
#*******************************************************************#
DATABASE safre_af
GLOBALS
    "INTB01003.4gl"
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
          ejecuta            CHAR(200),
          farch              DATE,
          f_arch             CHAR(06)

   OPTIONS INPUT WRAP,
           PROMPT LINE LAST,
           ACCEPT KEY CONTROL-I

##   DEFER INTERRUPT       

    CALL STARTLOG("INTB0153.log")

    LET ban               = 0
    LET numero_reg        = 0
    LET hoy_hoy = TODAY
    LET hora    = TIME
    INITIALIZE n_seguro, v_det, v_cza, v_ruta, ejecuta, fecha TO NULL

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
        WHERE v.layout_cod = 447

    OPEN WINDOW INTB0153 AT 2,2 WITH FORM "INTB01531" ATTRIBUTE(BORDER)
         DISPLAY " < ESC > Procesar                                          <",
                 " Ctrl-C > Salir    " AT 1,1 ATTRIBUTE(REVERSE)
                                                                                
         DISPLAY " INTB0153               RECHAZO DE REGISTR",
                 "O NO AFILIADOS                       "
                 AT 3,1 ATTRIBUTE(REVERSE)
         DISPLAY hoy_hoy  USING "DD-MM-YYYY " AT 3,66 ATTRIBUTE(REVERSE)

##         CONSTRUCT inter_noti ON   b.f_rechazo, b.rdeta_cod, b.n_seguro
         CONSTRUCT inter_noti ON   b.f_rechazo
                                FROM scr_1.*

                   ON KEY(ESC)
                      LET ban = 0
                      EXIT CONSTRUCT

                   ON KEY(CONTROL-C,INTERRUPT)
                         LET ban = 1
                         EXIT CONSTRUCT
         END CONSTRUCT 

         IF ban  = 0 THEN


          LET selec_tab =" SELECT b.f_rechazo,  b.rdeta_cod,  b.n_seguro, ",
                         " b.n_folio ",
                         " FROM   afi_solicitud a,  afi_rechaza_cert b ",
                         " WHERE  a.status_interno  IN (40,42)",
                         " AND    a.tipo_solicitud = 8 ",
                         " AND    a.n_seguro       = b.n_seguro ",
                         " AND    a.n_folio        = b.n_folio ",
                         " AND    a.fentcons       = b.f_rechazo ",
                         " AND    b.rdeta_cod      <> '20' ",
                         " AND ", inter_noti CLIPPED,
                         " AND    a.n_seguro IN(SELECT c.nss FROM int_ctr_carta c ",
                         " WHERE  b.f_rechazo= c.fecha_registro ",
                         " AND    b.n_seguro = c.nss ",
                         " AND    b.n_folio  = c.n_folio ",
                         " AND    b.tipo_solicitud= c.tipo_solicitud ",
                         " AND    c.docto_cod IN (30509, 30533) ",
                         --" AND    c.docto_cod = 30533 ",
                         " AND    c.edo_genera = 10)"

              LET farch    = inter_noti[12,21]

              IF farch IS NULL OR farch = " " THEN
                  LET f_arch = TODAY USING "mmddyy"
              ELSE
                  LET f_arch = farch USING "mmddyy"
              END IF

             LET v_arch = v_ruta CLIPPED,"/", "30533",
                          f_arch CLIPPED,hora1 CLIPPED,".",v_arch

             LET v_det  = v_ruta CLIPPED,"/", v_det  CLIPPED
             LET v_cza  = v_ruta CLIPPED,"/", v_cza  CLIPPED

{
             LET v_arch = "30533",
                          f_arch CLIPPED,hora1 CLIPPED,".",v_arch
             LET v_det  = v_det  CLIPPED
             LET v_cza  = v_cza  CLIPPED

}
             DISPLAY "GENERANDO INFORMACION.... AGUARDE UN MOMENTO..." at 19,1
                     ATTRIBUTE(REVERSE)

             LET leyenda_cza = "RECHAZO DE REGISTRO NO AFILIADO"

             START REPORT r_report TO v_det
             CALL detalle_notifica("33",447,2)

             START REPORT r_report TO v_cza
             CALL cabeza_notifica("33",leyenda_cza,447,1)


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
    CLOSE WINDOW INTB0153 
END MAIN
