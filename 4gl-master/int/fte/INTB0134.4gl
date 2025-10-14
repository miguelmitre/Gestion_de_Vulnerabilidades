#*******************************************************************#
#Proyecto          => Sistema de Afores.( MEXICO )                  #
#Propietario       => E.F.P.                                        #
#Programa          => INTB0134 Y INTB01001                          #
#Descripcion       => NOTIFICACION PENDIENTE DE TRASPASO Y REGISTRO #
#                  => EN ADMINISTRADORA                             #
#Sistema           => INT .                                         #
#Fecha             => 17 de abril del 2002   .                      #
#Por               => Laura Eugenia Cortes Guzman                   #
#*******************************************************************#
DATABASE safre_af
GLOBALS
    "INTB01001.4gl"
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
          f_arch             CHAR(06),
          enter              CHAR(1),

       reg_1     RECORD
            n_seguro            CHAR(11),
            motivo_rechazo      CHAR(03),
            fecha_presentacion  DATE,
            n_folio             DECIMAL(10,0),
            tipo_solicitud      SMALLINT
       END RECORD,

       reg_2     RECORD
            n_seguro            CHAR(11),
            motivo_rechazo      CHAR(03),
            fecha_presentacion  DATE,
            n_folio             DECIMAL(10,0),
            tipo_solicitud      SMALLINT
       END RECORD,

       reg_3     RECORD
            n_seguro            CHAR(11),
            motivo_rechazo      CHAR(03),
            fecha_presentacion  DATE,
            n_folio             DECIMAL(10,0),
            tipo_solicitud      SMALLINT
       END RECORD,

       reg_4     RECORD
            n_seguro            CHAR(11),
            motivo_rechazo      CHAR(03),
            fecha_presentacion  DATE,
            n_folio             DECIMAL(10,0),
            tipo_solicitud      SMALLINT
       END RECORD,

       reg_11    RECORD
          n_seguro          CHAR(11),     
          rfc               CHAR(13),     
          curp              CHAR(18),     
          paterno           CHAR(40),     
          materno           CHAR(40),     
          nombres           CHAR(40),     
          sexo              SMALLINT,     
          folio             DECIMAL(10,0),
          fecha_certifica   DATE,         
          tipo_solicitud    SMALLINT
       END RECORD,

       reg_22    RECORD
          n_seguro          CHAR(11),     
          rfc               CHAR(13),     
          curp              CHAR(18),     
          paterno           CHAR(40),     
          materno           CHAR(40),     
          nombres           CHAR(40),     
          sexo              SMALLINT,     
          folio             DECIMAL(10,0),
          fecha_certifica   DATE,         
          tipo_solicitud    SMALLINT
       END RECORD,

       reg_33    RECORD
          n_seguro          CHAR(11),     
          rfc               CHAR(13),     
          curp              CHAR(18),     
          paterno           CHAR(40),     
          materno           CHAR(40),     
          nombres           CHAR(40),     
          sexo              SMALLINT,     
          folio             DECIMAL(10,0),
          fecha_certifica   DATE,         
          tipo_solicitud    SMALLINT
       END RECORD,

       reg_44    RECORD
          n_seguro          CHAR(11),     
          rfc               CHAR(13),     
          curp              CHAR(18),     
          paterno           CHAR(40),     
          materno           CHAR(40),     
          nombres           CHAR(40),     
          sexo              SMALLINT,     
          folio             DECIMAL(10,0),
          fecha_certifica   DATE,         
          tipo_solicitud    SMALLINT
       END RECORD,

       reg_temp  RECORD
          n_seguro          CHAR(11),     
          rfc               CHAR(13),     
          curp              CHAR(18),     
          paterno           CHAR(40),     
          materno           CHAR(40),     
          nombres           CHAR(40),     
          sexo              SMALLINT,     
          folio             DECIMAL(10,0),
          fecha_certifica   DATE,         
          tipo_solicitud    SMALLINT,     
          fecha_presenta    DATE,         
          motivo_rechazo    CHAR(03),     
          proceso           CHAR(01)      
       END RECORD,
       moti_vo       CHAR(3),
       max_fecha         DATE

   OPTIONS INPUT WRAP,
           PROMPT LINE LAST,
           ACCEPT KEY CONTROL-I

   DEFER INTERRUPT       

    CALL STARTLOG ("INTB0134.log")

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE tmp_taa_pend

        CREATE TABLE tmp_taa_pend
        (n_seguro          CHAR(11),
         rfc               CHAR(13),     
         curp              CHAR(18),     
         paterno           CHAR(40),     
         materno           CHAR(40),     
         nombres           CHAR(40),     
         sexo              SMALLINT,     
         folio             DECIMAL(10,0),
         fecha_certifica   DATE,         
         tipo_solicitud    SMALLINT,     
         fecha_presenta    DATE,         
         motivo_rechazo    CHAR(03),     
         proceso           CHAR(01))

        DELETE FROM tmp_taa_pend
    WHENEVER ERROR STOP
      DATABASE safre_af

    LET ban               = 0
    LET numero_reg        = 0
    LET consecutivo_envio = 0
    LET hoy_hoy = TODAY
    LET hora    = TIME
    INITIALIZE fentcons, n_seguro, v_det, v_cza, v_ruta, ejecuta, fecha TO NULL
    INITIALIZE farch, f_arch TO NULL

    SELECT f.* INTO p_tabafore.*
        FROM tab_afore_local f

    SELECT c.ruta_envio INTO v_ruta 
        FROM seg_modulo c
        WHERE c.modulo_cod = "int"

    LET hoy = TODAY USING "mmddyy"
    LET hora1 = hora[1,2],hora[4,5],hora[7,8]

    SELECT v.nom_arch, arch_det, arch_cza 
        INTO  v_arch, v_det, v_cza
        FROM  tab_layout v
        WHERE v.layout_cod = 434

    OPEN WINDOW INTB0134 AT 2,2 WITH FORM "INTB01341" ATTRIBUTE(BORDER)
         DISPLAY " < ESC > Procesar                                          <",
                 " Ctrl-C > Salir    " AT 1,1 ATTRIBUTE(REVERSE)
                                                                                
         DISPLAY " INTB0134       PENDIENTE TRASPASO Y REGISTRO EN A",
                 "DMON.                        "
                 AT 3,1 ATTRIBUTE(REVERSE)
         DISPLAY hoy_hoy  USING "DD-MM-YYYY " AT 3,66 ATTRIBUTE(REVERSE)

         CONSTRUCT inter_noti ON   fecha_presentacion
                              FROM fecha_presentacion

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

               LET selec_tab1 = " SELECT  a.n_seguro, a.motivo_rechazo,   ",
                            " a.fecha_presentacion,b.n_folio,         ",
                            "  b.tipo_solicitud                       ",
                            " FROM taa_det_devol a, int_ctr_carta b   ",
                            " WHERE a.",inter_noti CLIPPED,
                            "   AND a.fecha_presentacion = b.fecha_registro ",
                            "   AND a.n_seguro           = b.nss            ",
                            "   AND b.edo_genera = 10                       ",
                            "   AND b.docto_cod          = 30227            "

             PREPARE apt_1 FROM selec_tab1
             DECLARE cur_1 CURSOR FOR apt_1
             FOREACH cur_1 INTO reg_1.*

                 SELECT a.n_seguro,  a.n_rfc,  a.n_unico, 
                        a.paterno,    a.materno,  a.nombres,
                        a.sexo, a.n_folio, MAX(a.fentcons), 
                        a.tipo_solicitud
                 INTO reg_11.*
                 FROM afi_solicitud a
                 WHERE a.n_seguro = reg_1.n_seguro
                   AND a.n_folio  = reg_1.n_folio 
                   AND a.tipo_solicitud  = reg_1.tipo_solicitud 
                   AND a.status_interno  IN(70,45,40,50)
                 GROUP BY 1,2,3,4,5,6,7,8,10

                 IF STATUS != NOTFOUND THEN
                    IF LENGTH(reg_1.motivo_rechazo) = 2 THEN
                       LET reg_1.motivo_rechazo = "0",reg_1.motivo_rechazo
                    END IF
                    INSERT INTO safre_tmp:tmp_taa_pend 
                           VALUES(reg_11.*,
                                  reg_1.fecha_presentacion,
                                  reg_1.motivo_rechazo,
                                  'D')
                 ELSE
                    SELECT MAX(a.fentcons) INTO max_fecha FROM afi_solicitud a
                    WHERE a.n_seguro = reg_1.n_seguro
                      AND a.status_interno  IN(70,45,40,50)

                    SELECT a.n_seguro,  a.n_rfc,  a.n_unico, 
                           a.paterno,    a.materno,  a.nombres,
                           a.sexo, a.n_folio, a.fentcons, 
                           a.tipo_solicitud
                    INTO reg_11.*
                    FROM afi_solicitud a
                    WHERE a.n_seguro = reg_1.n_seguro
                      AND a.fentcons = max_fecha
                      AND a.status_interno  IN(70,45,40,50)

                    IF STATUS != NOTFOUND THEN
                       --LET reg_1.motivo_rechazo = "0",reg_1.motivo_rechazo
                        IF LENGTH(reg_1.motivo_rechazo) = 2 THEN
                           LET reg_1.motivo_rechazo = "0",reg_1.motivo_rechazo
                        END IF
                       INSERT INTO safre_tmp:tmp_taa_pend 
                              VALUES(reg_11.*,
                                     reg_1.fecha_presentacion,
                                     reg_1.motivo_rechazo,
                                     'D')

                       UPDATE int_ctr_carta
                          SET int_ctr_carta.n_folio = reg_1.n_folio,
                              int_ctr_carta.tipo_solicitud = reg_11.tipo_solicitud
                        WHERE int_ctr_carta.nss = reg_1.n_seguro
                          AND int_ctr_carta.docto_cod = 30227
                          AND int_ctr_carta.edo_genera = 10
                    END IF
                 END IF

                INITIALIZE reg_11.* TO NULL
             END FOREACH

             INITIALIZE selec_tab1 TO NULL

             LET selec_tab1= "SELECT UNIQUE a.n_seguro, '000', a.fecha_presentacion,",
                             " b.n_folio, b.tipo_solicitud                   ",
                             " FROM taa_det_no_aten a, int_ctr_carta b       ",
                             " WHERE a.n_seguro = b.nss                      ",
                             "   AND b.docto_cod = 30227                     ",
                             "   AND b.edo_genera = 10                       ",
                             "   AND a.fecha_presentacion = b.fecha_registro ",
                             "   AND a.",inter_noti CLIPPED

             PREPARE apt_2 FROM selec_tab1
             DECLARE cur_2 CURSOR FOR apt_2
             FOREACH cur_2 INTO reg_2.*

                 SELECT a.n_seguro,  a.n_rfc,  a.n_unico, 
                        a.paterno,    a.materno,  a.nombres,
                        a.sexo, a.n_folio, MAX(a.fentcons), 
                        a.tipo_solicitud
                 INTO reg_22.*
                 FROM afi_solicitud a
                 WHERE a.n_seguro = reg_2.n_seguro
                   AND a.n_folio  = reg_2.n_folio 
                   AND a.tipo_solicitud  = reg_2.tipo_solicitud 
                   AND a.status_interno  IN(70,45,40,50)
                 GROUP BY 1,2,3,4,5,6,7,8,10

                 IF STATUS != NOTFOUND THEN
                        
#                 LET reg_2.motivo_rechazo = "0",reg_2.motivo_rechazo
                    IF LENGTH(reg_2.motivo_rechazo) = 2 THEN
                       LET reg_2.motivo_rechazo = "0",reg_2.motivo_rechazo
                    END IF
                    INSERT INTO safre_tmp:tmp_taa_pend 
                           VALUES(reg_22.*,
                                  reg_2.fecha_presentacion,
                                  reg_2.motivo_rechazo,
                                  'N')
                 ELSE
                     SELECT a.n_seguro,  a.n_rfc,  a.n_unico, 
                            a.paterno,    a.materno,  a.nombres,
                            a.sexo, a.n_folio, MAX(a.fentcons), 
                            a.tipo_solicitud
                     INTO reg_22.*
                     FROM afi_solicitud a
                     WHERE a.n_seguro = reg_2.n_seguro
                       AND a.tipo_solicitud  = reg_2.tipo_solicitud 
                       AND a.status_interno  IN(70,45,40,50)
                     GROUP BY 1,2,3,4,5,6,7,8,10
                     IF STATUS != NOTFOUND THEN
                        INSERT INTO safre_tmp:tmp_taa_pend
                                    VALUES(reg_22.*,
                                    reg_2.fecha_presentacion,
                                    reg_2.motivo_rechazo,
                                    'N')
                        UPDATE int_ctr_carta SET int_ctr_carta.n_folio =reg_22.folio
                        WHERE  int_ctr_carta.nss = reg_22.n_seguro

                     END IF
                 END IF

                INITIALIZE reg_22.* TO NULL
             END FOREACH

             INITIALIZE selec_tab1, farch TO NULL
             LET farch    = inter_noti[21,30]

             IF farch IS NOT NULL OR farch <> " " THEN

                LET selec_tab1= "SELECT UNIQUE a.n_seguro, b.rdeta_cod, b.f_rechazo,",
                                " a.n_folio, a.tipo_solicitud                ",
                                " FROM afi_solicitud a,afi_rechaza_cert b,   ",
                                " int_ctr_carta c                            ",
                                " WHERE a.n_seguro = b.n_seguro              ",
                                "   AND a.status_interno = 50                ",
                                "   AND a.tipo_solicitud = 2                 ",
                                "   AND a.fentcons = b.f_rechazo             ",
                                "   AND a.fentcons  = '",farch CLIPPED,"' ",
                                "   AND c.nss = a.n_seguro                   ",
                                "   AND c.docto_cod = 30227                  ",
                                "   AND c.edo_genera = 10                    ",
                                "   AND c.fecha_registro = a.fentcons        "

                PREPARE apt_3 FROM selec_tab1
                DECLARE cur_3 CURSOR FOR apt_3
                FOREACH cur_3 INTO reg_3.*

                    SELECT UNIQUE a.n_seguro,  a.n_rfc,  a.n_unico, 
                           a.paterno,    a.materno,  a.nombres,
                           a.sexo, a.n_folio, MAX(a.fentcons), 
                           a.tipo_solicitud
                    INTO reg_33.*
                    FROM afi_solicitud a
                   WHERE a.n_seguro = reg_3.n_seguro
                     AND a.n_folio  = reg_3.n_folio 
                     AND a.tipo_solicitud  = reg_3.tipo_solicitud 
                     AND a.status_interno  = 50
                   GROUP BY 1,2,3,4,5,6,7,8,10

                     IF STATUS != NOTFOUND THEN
                        --LET reg_3.motivo_rechazo = "0",reg_3.motivo_rechazo
                        IF LENGTH(reg_3.motivo_rechazo) = 2 THEN
                           LET reg_3.motivo_rechazo = "0",reg_3.motivo_rechazo
                        END IF
                        INSERT INTO safre_tmp:tmp_taa_pend 
                               VALUES(reg_33.*,
                                      reg_3.fecha_presentacion,
                                      reg_3.motivo_rechazo,
                                      'P')
                     END IF
                     INITIALIZE reg_33.* TO NULL

                END FOREACH 
             ELSE
                LET selec_tab1= "SELECT a.n_seguro, b.rdeta_cod, b.f_rechazo,",
                                " a.n_folio, a.tipo_solicitud                ",
                                " FROM afi_solicitud a,afi_rechaza_cert b,   ",
                                " int_ctr_carta c                            ",
                                " WHERE a.n_seguro = b.n_seguro              ",
                                "   AND a.status_interno = 50                ",
                                "   AND a.tipo_solicitud = 2                 ",
                                "   AND a.fentcons = b.f_rechazo             ",
                                "   AND b.f_rechazo =",farch CLIPPED,
                                "   AND c.nss = a.n_seguro                   ",
                                "   AND c.docto_cod = 30227                  ",
                                "   AND c.edo_genera = 10                    ",
                                "   AND c.fecha_registro = a.fentcons        "

                PREPARE apt_4 FROM selec_tab1
                DECLARE cur_4 CURSOR FOR apt_4
                FOREACH cur_4 INTO reg_4.*

                    SELECT a.n_seguro,  a.n_rfc,  a.n_unico, 
                           a.paterno,    a.materno,  a.nombres,
                           a.sexo, a.n_folio, MAX(a.fentcons), 
                           a.tipo_solicitud
                    INTO reg_33.*
                    FROM afi_solicitud a
                   WHERE a.n_seguro        = reg_4.n_seguro
                     AND a.n_folio         = reg_4.n_folio 
                     AND a.tipo_solicitud  = reg_4.tipo_solicitud 
                     AND a.status_interno  = 50

                     IF STATUS != NOTFOUND THEN
                        --LET reg_3.motivo_rechazo = "0",reg_4.motivo_rechazo
                        IF LENGTH(reg_4.motivo_rechazo) = 2 THEN
                           LET reg_4.motivo_rechazo = "0",reg_4.motivo_rechazo
                        END IF
                        INSERT INTO safre_tmp:tmp_taa_pend 
                               VALUES(reg_44.*,
                                      reg_4.fecha_presentacion,
                                      reg_4.motivo_rechazo,
                                      'P')
                     END IF
                     INITIALIZE reg_44.* TO NULL
                END FOREACH 

             END IF

             INITIALIZE selec_tab1, farch TO NULL
             LET selec_tab1= " SELECT * FROM safre_tmp:tmp_taa_pend "

 #display selec_tab1 CLIPPED
 #prompt "3 " for enter
 #exit program

             LET farch    = inter_noti[21,30]
             IF farch IS NULL OR farch = " " THEN
                 LET f_arch = TODAY USING "mmddyy"
             ELSE
                 LET f_arch = farch USING "mmddyy"
             END IF


             LET v_arch = v_ruta CLIPPED,"/", "30227",
                          f_arch CLIPPED,hora1 CLIPPED,".",v_arch
             LET v_det  = v_ruta CLIPPED,"/", v_det  CLIPPED
             LET v_cza  = v_ruta CLIPPED,"/", v_cza  CLIPPED

{
             LET v_arch = "30227",
                          f_arch CLIPPED,hora1 CLIPPED,".",v_arch
             LET v_det  = v_det  CLIPPED
             LET v_cza  = v_cza  CLIPPED
}
             DISPLAY "GENERANDO INFORMACION.... AGUARDE UN MOMENTO..." at 19,1
                     ATTRIBUTE(REVERSE)

             LET leyenda_cza = "PENDIENTE DE TRASPASO Y REGISTRO EN ",
                               "ADMINISTRADORA DE FONDOS PARA EL RETIRO"

             START REPORT r_report TO v_det
             CALL detalle_notifica_34("27",434,2)

             START REPORT r_report TO v_cza
             CALL cabeza_notifica_34("27",leyenda_cza,434,1)

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
    CLOSE WINDOW INTB0134 
END MAIN
