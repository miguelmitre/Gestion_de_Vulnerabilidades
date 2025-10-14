#*******************************************************************#
#Proyecto          => Sistema de Afores.( MEXICO )                  #
#Propietario       => E.F.P.                                        #
#Programa          => INTB0158 Y INTB01010                          #
#Descripcion       => NOTIFICACION DE CONSTANCIA DE TRASPASO Y RE-- #
#                  => GISTRO. POR INTERNET.                         #
#Sistema           => INT .                                         #
#Fecha             => 18 de Agosto del 2005  .                      #
#Por               => Laura Eugenia Cortes Guzman                   #
#Fecha Modif.      => 25 de septiembre del 2005                     #
#Por               => Laura Eugenia Cortes Guzman                   #
#Req: 1046         => JCPV  Sleccion por Tipo Solicitud.            #
#Reg 1148			     =>  FSR se quita origen 57                       #
#CPL-1686          => FSR 18/08/2014 archivo paso por usuario       #
#*******************************************************************#
DATABASE safre_af
GLOBALS
    "INTB01010.4gl"
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
          reci               CHAR(1),
          nss                CHAR(11),
          hora1              CHAR(06),
          ejecuta            CHAR(200),
          farch              DATE,
          f_arch             CHAR(06),
          vtipo_traspaso     CHAR(15),    #1046
          vtipo_solicitud    CHAR(10)     #1046
   

   OPTIONS INPUT WRAP,
           PROMPT LINE LAST,
           ACCEPT KEY CONTROL-I

   DEFER INTERRUPT       
   CALL STARTLOG(FGL_GETENV("USER")||".INTB0158.log")       

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
        WHERE v.layout_cod = 457

    IF ban <> 0 THEN
       ERROR "ERROR: Faltan comisiones "
       SLEEP 3
       EXIT PROGRAM
    END IF

    OPEN WINDOW INTB0158 AT 2,2 WITH FORM "INTB01581" ATTRIBUTE(BORDER)
      DISPLAY " < ESC > Procesar                                          <",
              " Ctrl-C > Salir    " AT 1,1 ATTRIBUTE(REVERSE)
                                                                             
      DISPLAY " INTB0158        CONSTANCIA DE TRASPASO DE ",
              "CORREO ELECTRONICO                  "
              AT 3,1 ATTRIBUTE(REVERSE)
      DISPLAY hoy_hoy  USING "DD-MM-YYYY " AT 3,66 ATTRIBUTE(REVERSE)

      CONSTRUCT inter_noti ON   a.fecha_mov_banxico
                           FROM fecha_mov_banxico

                ON KEY(ESC)
                   LET ban = 0
                   EXIT CONSTRUCT

                ON KEY(INTERRUPT,CONTROL-C)
                      LET ban = 1
                      EXIT CONSTRUCT
      END CONSTRUCT 

      IF ban  = 0 THEN
         WHILE TRUE
              PROMPT "(A)38,55,72,74 NORMAL; ",
                 #    "(B)57 ASIGINT; ", 1148
                     "(S)Salir: " FOR enter
              IF enter MATCHES "[aA/sS]" THEN
                 IF enter MATCHES "[aA]" THEN
                    LET vtipo_traspaso  = "38,55,72,74"
                    LET vtipo_solicitud = "9,11,16,18"
                    EXIT WHILE
                 ELSE
                  { IF enter MATCHES "[Bb]" THEN
                      LET vtipo_traspaso  = "57"
                      LET vtipo_solicitud = "9,11"
                      EXIT WHILE
                   ELSE } #1148                    
                     ERROR "Proceso cancelado. . ."
                     SLEEP 3
                     ERROR ""
                     EXIT PROGRAM
                  -- END IF
                 END IF
              ELSE
                 ERROR "Opciones:(A)NORMAL;(S) Salir"
                 SLEEP 3
                 ERROR ""
              END IF
            END WHILE

         LET selec_tab1= " SELECT a.fecha_mov_banxico, a.cve_ced_cuenta, ",
                         " a.cve_recep_cuenta, b.n_seguro, b.n_rfc,      ",
                         " b.n_unico, b.paterno,  b.materno,  b.nombres, ",
                         " b.sexo, b.n_folio, b.fentcons, b.tipo_solicitud,",
                         " b.fecha_elaboracion, b.fena, b.nacionalidad, ",
                         " b.tip_prob,b.fecha_1a_afil, ",
                         " a.importe_bono, a.fecha_red_bono ",
                         " FROM  taa_viv_recepcion a, afi_mae_afiliado b,",
                         "       int_ctr_carta c  "

         LET selec_tab1= selec_tab1 CLIPPED,
                         " WHERE a.nss           = b.n_seguro",
                         " AND   a.ident_operacion = '09'    ",
                         " AND   b.n_seguro = c.nss          ",
                         " AND  a.fecha_mov_banxico=c.fecha_registro ",
                         " AND  c.docto_cod = 30280          ",
                         " AND  c.edo_genera = 10            ",
                        -- " AND  b.tipo_solicitud IN(9,11,16,18) ",
                         " AND b.tipo_solicitud IN(", vtipo_solicitud CLIPPED, ")",
                         " AND  b.fentcons IS NOT NULL       ",
                        -- " AND  a.tipo_traspaso  IN ('38','55','57','72','74') ",
                         " AND a.tipo_traspaso in (", vtipo_traspaso CLIPPED, ")",
                         " AND ",inter_noti CLIPPED

          LET farch    = inter_noti[20,29]
          IF farch IS NULL OR farch = " " THEN
                      LET f_arch = TODAY USING "mmddyy"
          ELSE
                      LET f_arch = farch USING "mmddyy"
          END IF
{
          LET v_arch = "30280",
                       f_arch CLIPPED,hora1 CLIPPED,".",v_arch
          LET v_cza  = v_cza  CLIPPED
          LET v_det  = v_det  CLIPPED
}
          LET v_arch = v_ruta CLIPPED,"/", "30280",
                       f_arch CLIPPED,hora1 CLIPPED,".",v_arch
          LET v_cza  = v_ruta CLIPPED,"/", v_cza  CLIPPED, "_", usuario CLIPPED #CPL-1686
          LET v_det  = v_ruta CLIPPED,"/", v_det  CLIPPED, "_", usuario CLIPPED #CPL-1686

       DISPLAY "GENERANDO INFORMACION.... AGUARDE UN MOMENTO..." at 19,1
                  ATTRIBUTE(REVERSE)

          LET leyenda_cza = "CONSTANCIA DE TRASPASO "

          START REPORT r_report TO v_det
          CALL detalle_notifica_58("80",457,2)

          START REPORT r_report TO v_cza
          CALL cabeza_notifica_58("80",leyenda_cza,457,1)

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
    CLOSE WINDOW INTB0158 

END MAIN
