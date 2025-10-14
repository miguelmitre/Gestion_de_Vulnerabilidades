###############################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                           #
#Owner             => E.F.P.                                                  #
#Programa          => COMC009                                                 #
#Descripcion       => REPORTE DE PRODUCCION GLOBAL                            #
#Fecha             => 15 Marzo 2004.                                          #
#By                => JOSE ALEJANDRO RAMIREZ.                                 #
#Sistema           => COM.                                                    #
#Modifico          => ISABEL FONSECA FRIAS                                    #
#Fecha             => 26-Agosto-2005                                          #
#Descripcion       => (v1) se desplegara en pantalla el codven en lugar de nss#
###############################################################################
DATABASE safre_af
GLOBALS
   DEFINE

      g_param_dis     RECORD LIKE seg_modulo.* ,
      w_codigo_afore  LIKE tab_afore_local.codigo_afore,
      g_usuario       CHAR (08),


      g_reg0 RECORD
        cod_tipo_prom INTEGER,
        coduni_n1     CHAR(10),
        fecha_corte   DATE
      END RECORD,

      g_reg2 RECORD
        paterno       CHAR(40),
        materno       CHAR(40),
        nombres       CHAR(40)
      END RECORD,


      g_reg ARRAY[5000] OF RECORD
         codven             LIKE com_comis_detalle.codven,
         cod_tipo_prom      LIKE com_comis_detalle.cod_tipo_prom,
         coduni_n1          LIKE com_comis_detalle.coduni_n1,
         fecha_corte        LIKE com_comis_detalle.fecha_corte,
         vtot_afil          INTEGER,
         vnum_sm            decimal(12,2), --LIKE com_comis_detalle.num_sm,
         vprom_sm           decimal(12,2),     --LIKE com_comis_detalle.num_sm,
         vmonto_comi     decimal(16,2), --LIKE com_comis_detalle.monto_comision,
         vedo               LIKE com_comis_detalle.estado_comision
      END RECORD,
      g_reg3 ARRAY[5000] OF RECORD
         n_folio            LIKE com_comis_detalle.n_folio,
         nss                LIKE com_comis_detalle.nss,
         tipo_solicitud     LIKE com_comis_detalle.tipo_solicitud,
         fentcons           LIKE com_comis_detalle.fentcons,
         fecha_corte        LIKE com_comis_detalle.fecha_corte,
         salario_base_comis LIKE com_comis_detalle.salario_base_comis,
         num_sm             LIKE com_comis_detalle.num_sm,
         monto_comision     LIKE com_comis_detalle.monto_comision,
         estado_comision    LIKE com_comis_detalle.estado_comision
      END RECORD,

      g_reg33 ARRAY[5000] OF RECORD
         n_folio            LIKE afi_solicitud.n_folio,
         n_seguro           LIKE afi_solicitud.n_seguro,
         tipo_solicitud     LIKE afi_solicitud.tipo_solicitud,
         frecafor           LIKE afi_solicitud.fentcons,
         salario_base_comis LIKE afi_solicitud.salario_base_comis,
         status_interno     LIKE afi_solicitud.status_interno,
         estado_desc        LIKE afi_status_int.estado_desc
      END RECORD,

   g_reg4 ARRAY[6] OF RECORD
      --  seguro         LIKE pro_mae_promotor.seguro,     --(v1)
      codven         LIKE pro_mae_promotor.codven,
      paterno        LIKE pro_mae_promotor.paterno,
      materno        LIKE pro_mae_promotor.materno,
      nombres        LIKE pro_mae_promotor.nombres
   END RECORD,

   g_reg5 ARRAY[6] OF RECORD
     -- seguro         LIKE pro_mae_promotor.seguro,       --(v1)
      codven         LIKE pro_mae_promotor.codven,
      nombre         CHAR(50)
   END RECORD,

   g_reg6 ARRAY[6] OF RECORD
      n_folio        LIKE afi_solicitud.n_folio,
      n_seguro       LIKE afi_solicitud.n_seguro,
      tipo_solicitud LIKE afi_solicitud.tipo_solicitud,
      paterno        LIKE afi_solicitud.paterno,
      materno        LIKE afi_solicitud.materno,
      nombres        LIKE afi_solicitud.nombres,
      observacion    LIKE afi_rechaza_cert.observacion
   END RECORD,


   g_reg7 ARRAY[6] OF RECORD
      n_folio        LIKE afi_solicitud.n_folio,
      n_seguro       LIKE afi_solicitud.n_seguro,
      tipo_solicitud LIKE afi_solicitud.tipo_solicitud,
      nombre         CHAR(50),
      observacion     LIKE afi_rechaza_cert.observacion
   END RECORD,

   nombre CHAR(50),

      vmenu     CHAR(01),
      aux_pausa CHAR(1),
      HOY       DATE,
      SW_1      SMALLINT,
      cla_sel   CHAR(500),
      cla_sel2  CHAR(500),
      vaccion   smallint,
      cla_where CHAR(800),
      vcomando  SMALLINT,
      opc       CHAR(01),
      total     DECIMAL(16,2),
      pagada    DECIMAL(16,2),
      registros INTEGER,
      longitud  integer,

    nuevo RECORD
      codven             LIKE com_comis_resumen.codven,
      cod_tipo_prom      LIKE com_comis_detalle.cod_tipo_prom,
      coduni_n1          LIKE com_comis_detalle.coduni_n1,
      fecha_corte        LIKE com_comis_detalle.fecha_corte,
      total_afiliados    LIKE com_comis_resumen.total_afiliados,
      total_sm           decimal(12,2), --LIKE com_comis_resumen.total_sm,
      promedio_sm        decimal(12,2),   --LIKE com_comis_resumen.promedio_sm,
      total_comision     LIKE com_comis_resumen.total_comision,
      paterno            CHAR(40),
      materno            CHAR(40),
      nombres            CHAR(40),
      estado_comision    LIKE com_comis_detalle.estado_comision
    END RECORD,

    l_record_1  ARRAY[2000] OF RECORD
         n_folio            LIKE afi_solicitud.n_folio,
         n_seguro           LIKE afi_solicitud.n_seguro,
         tipo_solicitud     LIKE afi_solicitud.tipo_solicitud,
         frecafor           LIKE afi_solicitud.fentcons,
         vfecha_corte       LIKE com_comis_detalle.fecha_corte,
         salario_base_comis LIKE afi_solicitud.salario_base_comis,
         num_sm             LIKE com_comis_detalle.num_sm,
         monto_comision     LIKE com_comis_detalle.monto_comision
    END RECORD,

         vregis             INTEGER,
         cont_de_registros  INTEGER,
         sumtot_afil        INTEGER,
         sumtot_sm          DECIMAL(8,2),
         sumtot_promsm      DECIMAL(6,2),
         sumtot_comi        DECIMAL(12,2),
         vnom_jefe          CHAR(50)


END GLOBALS

MAIN
        OPTIONS PROMPT LINE LAST,
                INPUT WRAP,
                ACCEPT KEY control-o

        DEFER INTERRUPT

        SELECT  ruta_rescate
        INTO    g_param_dis.ruta_rescate
        FROM    seg_modulo
        WHERE   modulo_cod='com'

      --LET g_param_dis.ruta_listados="/home/aramirez/ACTINVE/new_version" --ojo
      --LET g_param_dis.ruta_listados="/safre/com/fte/alex/pago_comer" --ojo

        SELECT  codigo_afore,USER
        INTO    w_codigo_afore,g_usuario
        FROM    tab_afore_local

        LET HOY = TODAY
        LET vregis = 0

        OPEN WINDOW ventana_1 AT 2,2 WITH FORM "COMC0091" ATTRIBUTE( BORDER)
        DISPLAY " COMC009            CONSULTA  DE  PRODUCCION  GLOBAL                          " AT 3,1 ATTRIBUTE(REVERSE)

        DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

        DISPLAY "                                                                               " AT 7,1 ATTRIBUTE(REVERSE)

        DISPLAY "                                                                               " AT 09,1 ATTRIBUTE(REVERSE)

        DISPLAY "                                                                               " AT 19,1 ATTRIBUTE(REVERSE) 

        MENU "CONSULTA"
           COMMAND "Consulta" "Consulta Comisiones"
              CALL Inicializa2()
              CALL Consulta()
           COMMAND "Salir" "Salir del Programa"
              EXIT MENU
        END MENU
        CLOSE WINDOW ventana_1
END MAIN

FUNCTION Inicializa()
    define i smallint
        LET sw_1 = 0
        INITIALIZE g_reg TO NULL


        for i=1 to 9
           DISPLAY g_reg[i].* TO scr_1[i].*
        end for

        LET total = 0
        LET registros = 0
        DISPLAY registros,total TO scr_3.*

        CLEAR SCREEN
END FUNCTION

FUNCTION Inicializa2()
   LET cla_where=NULL
   LET cla_sel=NULL
   INITIALIZE g_reg0.* TO NULL    --g_reg2.* ojo
   DISPLAY BY NAME g_reg0.*        --,g_reg2.* ojo
END FUNCTION
FUNCTION Consulta()
   DEFINE
      pat CHAR(40),
      mat char(40),
      nom char(40),
      pos SMALLINT

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " [ESC] Procesar " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " [Ctrl-C] Salir " AT 1,63 ATTRIBUTE(REVERSE)


      LET INT_FLAG = FALSE
      CONSTRUCT cla_where
         ON    a.cod_tipo_prom,
               a.coduni_n1,
               a.fecha_corte
         FROM  com_comis_detalle.cod_tipo_prom,
               com_comis_detalle.coduni_n1,
               com_comis_detalle.fecha_corte

         ON KEY (ESC)
            LET vcomando = 2
            EXIT CONSTRUCT
         ON KEY (INTERRUPT)
            LET vcomando=1
            EXIT CONSTRUCT
       END CONSTRUCT



       IF vcomando = 1 THEN
          LET INT_FLAG = FALSE
          ERROR "Operacion abortada"
          LET vcomando = 0
          RETURN
       END IF

       LET cla_sel ="SELECT a.cod_tipo_prom,",
                           "a.coduni_n1,",
                           "a.fecha_corte,",
                           "a.codven ",
                    "FROM   com_comis_detalle a ",
                   " WHERE ",cla_where CLIPPED,
                   " ORDER  BY 2" CLIPPED

         PREPARE claexe2 FROM cla_sel
         DECLARE cursor_2 SCROLL CURSOR FOR claexe2
         OPEN cursor_2

         CALL primer_row()
   DISPLAY " [Ctrl-B] Descrip estado     " AT 6,50 ATTRIBUTE(REVERSE)
   DISPLAY " [Ctrl_N] Nombre Promotor     [Ctrl-I] Imprime    [Ctrl-P] Clientes promotor" AT 7,01 ATTRIBUTE(REVERSE)    
         CALL ver_arreglo()

END FUNCTION

FUNCTION primer_row()
   FETCH FIRST cursor_2 INTO g_reg0.*      --,g_reg2.* ojo
   IF STATUS=100 THEN
      ERROR "No hay registros en esta direccion"
   ELSE
      CALL despliega_row()
   END IF
END FUNCTION

FUNCTION despliega_row()
 --DISPLAY BY NAME g_reg0.*     --g_reg2 ojo
END FUNCTION

#------------------------------------------------------------------
FUNCTION imprime()

  DEFINE hora       CHAR (08)
  DEFINE G_IMPRE    CHAR(300)
  DEFINE G_LISTA    CHAR(300)
  DEFINE impresion  CHAR(300)
  DEFINE i          SMALLINT
  DEFINE vdivide    CHAR(1)


  LET hora = TIME


    LET G_IMPRE = g_param_dis.ruta_rescate CLIPPED,"/",g_usuario CLIPPED,
                              ".RCOMC009_ADEL",HOY USING "DDMMYYYY"
--                              ,"_",hora CLIPPED

    LET G_LISTA = g_param_dis.ruta_rescate CLIPPED,"/",g_usuario CLIPPED,
                              ".ACOMC009_ADEL",HOY USING "DDMMYYYY"
--                                ,"_",hora CLIPPED

    -- PARA ARCHIVO
    START REPORT rpt_cuenta_arc TO G_LISTA

    -- PARA IMPRESION
    START REPORT rpt_cuenta_imp TO G_IMPRE
    LET cont_de_registros = 0

    LET sumtot_afil    = 0
    LET sumtot_sm      = 0
    LET sumtot_promsm  = 0
    LET sumtot_comi    = 0

    --Detalle de folios    -----------------------------------------------
  --FOR i=1 TO 300
    FOR i=1 TO 3000

        LET nuevo.codven                = g_reg[i].codven
        LET nuevo.cod_tipo_prom         = g_reg[i].cod_tipo_prom
        LET nuevo.coduni_n1             = g_reg[i].coduni_n1
        LET nuevo.fecha_corte           = g_reg[i].fecha_corte
        LET nuevo.total_afiliados       = g_reg[i].vtot_afil
        LET nuevo.total_sm              = g_reg[i].vnum_sm
        LET nuevo.promedio_sm           = g_reg[i].vprom_sm
        LET nuevo.total_comision        = g_reg[i].vmonto_comi
        LET nuevo.estado_comision       = g_reg[i].vedo

      IF nuevo.codven IS NULL OR
         nuevo.codven = ' ' THEN
         EXIT FOR
      END IF

      --VEREMOS

      LET nombre = ' '
      LET vnom_jefe     = ' '
      LET nuevo.paterno = ' '
      LET nuevo.materno = ' '
      LET nuevo.nombres = ' '

      SELECT paterno,materno,nombres
      INTO   nuevo.paterno,nuevo.materno,nuevo.nombres
      FROM   pro_mae_promotor
      -- issa WHERE  cod_promotor = g_reg[i].codven
      WHERE  codven = g_reg[i].codven

      IF     nuevo.nombres IS NULL THEN
                LET nuevo.paterno = ' '
                LET nuevo.materno = ' '
                LET nuevo.nombres = ' '
      END IF


      IF nuevo.paterno = ' ' AND nuevo.materno = ' ' THEN
         -- Buscamos si se trata de un jefe
         LET vnom_jefe = ' '
         SELECT nombre_resp_uni
         INTO   vnom_jefe
         FROM   com_respon_unidad
         WHERE  cod_resp_uni = g_reg[i].codven
      END IF


      LET vregis = vregis + 1

      --PARA EL ARCHIVO
      OUTPUT TO REPORT rpt_cuenta_arc(nuevo.*)

      -- PARA LA IMPRESION
      OUTPUT TO REPORT rpt_cuenta_imp(g_reg0.*,nuevo.*,vnom_jefe)

   END FOR

   FINISH REPORT rpt_cuenta_arc
   FINISH REPORT rpt_cuenta_imp
   ERROR "LISTADO 1 GENERADO...."
   SLEEP 2
   ERROR ""


--  LET impresion = "vi ",G_IMPRE
  LET impresion = "lp ",G_IMPRE
  RUN impresion

END FUNCTION


REPORT rpt_cuenta_arc(nuevo)
  DEFINE
    nuevo RECORD
      codven             LIKE com_comis_resumen.codven,
      cod_tipo_prom      LIKE com_comis_detalle.cod_tipo_prom,
      coduni_n1          LIKE com_comis_detalle.coduni_n1,
      fecha_corte        LIKE com_comis_detalle.fecha_corte,
      total_afiliados    LIKE com_comis_resumen.total_afiliados,
      total_sm           LIKE com_comis_resumen.total_sm,
      promedio_sm        LIKE com_comis_resumen.promedio_sm,
      total_comision     LIKE com_comis_resumen.total_comision,
      paterno            CHAR(40),
      materno            CHAR(40),
      nombres            CHAR(40),
      estado_comision    LIKE com_comis_detalle.estado_comision
    END RECORD,

    nombres            CHAR(60),
    cla                CHAR(250)

  FORMAT
     PAGE HEADER
      PRINT COLUMN 001,"Num promotor |",
            COLUMN 015,"Nombre promotor |",
            COLUMN 032,"Tipo promo|",
            COLUMN 043,"Centro costos|",
            COLUMN 057,"Fecha corte|",
            COLUMN 069,"Total Afi|",
            COLUMN 079,"Total sm|",
            COLUMN 088,"Prom  sm|",
            COLUMN 097,"Comision |",
            COLUMN 107,"Estado|"

      ON EVERY ROW
         LET nombres = ""

         IF LENGTH(nuevo.codven) < 10 THEN
            SELECT nombre_resp_uni
            INTO   nombres
            FROM   com_respon_unidad
            WHERE  cod_resp_uni = nuevo.codven
         ELSE
            LET cla = "SELECT trim(paterno)||' '||",
                             "trim(materno)||' '||",
                             "trim(nombres) ",
                      "FROM   pro_mae_promotor ",
                      -- issa "WHERE  cod_promotor = ",nuevo.codven
                      "WHERE  codven = ",nuevo.codven

            PREPARE claexe3 FROM cla
            DECLARE cur3 CURSOR FOR claexe3
            OPEN cur3
            FETCH cur3 INTO nombres
            CLOSE cur3
         END IF

      PRINT COLUMN 001,nuevo.codven CLIPPED,                    "|",
            COLUMN 015,nombres CLIPPED,                         "|",
            COLUMN 076,nuevo.cod_tipo_prom,                     "|",
            COLUMN 104,nuevo.coduni_n1,                         "|",
            COLUMN 118,nuevo.fecha_corte  USING 'DD/MM/YYYY',   "|",
            COLUMN 130,nuevo.total_afiliados,                   "|",
            COLUMN 191,nuevo.total_sm,                          "|",
            COLUMN 149,nuevo.promedio_sm,                       "|",
            COLUMN 158,nuevo.total_comision,                    "|",
            COLUMN 168,nuevo.estado_comision,                   "|"

END REPORT

REPORT rpt_cuenta_imp(g_reg0,nuevo,vnom_jf)

  DEFINE
    g_reg0 RECORD
        cod_tipo_prom SMALLINT,
        coduni_n1     CHAR(10),
        fecha_corte   DATE
    END RECORD,

    g_reg2 RECORD
        paterno       CHAR(40),
        materno       CHAR(40),
        nombres       CHAR(40)
    END RECORD,

    nuevo RECORD
        codven             LIKE com_comis_resumen.codven,
        cod_tipo_prom      LIKE com_comis_detalle.cod_tipo_prom,
        coduni_n1          LIKE com_comis_detalle.coduni_n1,
        fecha_corte        LIKE com_comis_detalle.fecha_corte,
        total_afiliados    LIKE com_comis_resumen.total_afiliados,
        total_sm           LIKE com_comis_resumen.total_sm,
        promedio_sm        LIKE com_comis_resumen.promedio_sm,
        total_comision     LIKE com_comis_resumen.total_comision,
        paterno            CHAR(40),
        materno            CHAR(40),
        nombres            CHAR(40),
        estado_comision    LIKE com_comis_detalle.estado_comision
    END RECORD,

         nombre             CHAR(60),
         vnom_jf            CHAR(60)

 OUTPUT
      TOP MARGIN 0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   90

  FORMAT
     PAGE HEADER

      PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
      PRINT COLUMN 02,'\033(s7B',"Pantalla COMC009      ",today USING "dd-mm-yyyy",'\033(s0B'

      SKIP 2 LINE
      PRINT COLUMN 80,'\033(s7B',"Reporte de Produccion Global",'\033(s0B'
      PRINT COLUMN 02,'\033(s7B',"Fecha de Corte:",'\033(s0B',
            COLUMN 18,nuevo.fecha_corte USING 'DD-MM-YYYY'
      SKIP 1 LINE


      PRINT COLUMN  002,'__________________________________________________',
                        '__________________________________________________',
                        '__________________________________________________',
                        '______________________________________'

            SKIP 1 LINE


      PRINT COLUMN 02,'\033(s7B',"Num promotor  Nombre promotor                              Tipo prom   Grp Venta   Fecha corte   Total afiliaciones  Total sm        Prome. sm            Tot comi  Estado",'\033(s0B'
      PRINT COLUMN  002,'__________________________________________________',
                        '__________________________________________________',
                        '__________________________________________________',
                        '______________________________________'
            SKIP 1 LINE


      ON EVERY ROW

      IF    vnom_jf <> ' ' THEN
            LET  nombre = vnom_jf
      ELSE
            LET   nombre =  nuevo.paterno CLIPPED||' '||
                            nuevo.materno CLIPPED||' '||
                            nuevo.nombres CLIPPED
      END IF

      PRINT COLUMN 02,nuevo.codven CLIPPED,
            COLUMN 15,nombre CLIPPED,
            COLUMN 60,nuevo.cod_tipo_prom CLIPPED,
            COLUMN 75,nuevo.coduni_n1 CLIPPED,
            COLUMN 85,nuevo.fecha_corte USING 'DD-MM-YYYY',
            COLUMN 095,nuevo.total_afiliados,
            COLUMN 115,nuevo.total_sm USING "#####,##&.&&",
            COLUMN 135,nuevo.promedio_sm USING "#####&.&&",
            COLUMN 148,nuevo.total_comision USING "#####,###,##&.&&",
            COLUMN 159,nuevo.estado_comision CLIPPED

      LET sumtot_afil = sumtot_afil + nuevo.total_afiliados
      LET sumtot_sm = sumtot_sm + nuevo.total_sm
      LET sumtot_promsm = sumtot_promsm + nuevo.promedio_sm
      LET sumtot_comi = sumtot_comi + nuevo.total_comision
      LET cont_de_registros = cont_de_registros + 1

      ON LAST ROW

       SKIP 1 LINE
       PRINT COLUMN 002,"TOTAL DE REGISTROS:                                 ",cont_de_registros,"                                   ",sumtot_afil USING "##,##&.&&","        ",sumtot_sm USING "###,##&.&&","         ",sumtot_promsm,"    ",sumtot_comi USING "#####,###,##&.&&"

END REPORT

FUNCTION ver_arreglo()
   DEFINE
      pat    CHAR(40),
      mat    char(40),
      nom    char(40),
      pos    SMALLINT,
      fhasta DATE,
      i      SMALLINT

       LET total = 0
       LET pagada = 0
       LET registros = 0

   LET cla_sel2="SELECT a.codven,a.cod_tipo_prom,",
                       "a.coduni_n1,a.fecha_corte,",
                       "count(*),sum(a.num_sm),AVG(a.num_sm),",
                       "sum(a.monto_comision),estado_comision ",
                "FROM   com_comis_detalle a ",
                "WHERE  ",cla_where CLIPPED,
               " GROUP BY 1,2,3,4,9 " CLIPPED,
               " ORDER BY 1,4,3,2 " CLIPPED

         ERROR "Buscando Informacion"

         PREPARE claexe FROM cla_sel2
         DECLARE cursor_1111 CURSOR FOR claexe
         LET pos = 1
         FOREACH cursor_1111 INTO g_reg[pos].*
                 LET total = total + g_reg[pos].vmonto_comi
                 LET registros = registros  + 1
                 LET pos = pos + 1
                 IF pos >= 9000 THEN
                    ERROR "Sobrepaso la capacidad del arreglo"
                    EXIT FOREACH
                 END IF
         END FOREACH

         DISPLAY registros,total TO scr_3.*

         ERROR ""

         CALL  SET_COUNT(pos-1)

         IF (pos-1) >= 1 THEN
            DISPLAY ARRAY g_reg TO scr_1.*

               ON KEY (CONTROL-I)
                  CALL imprime()
               ON KEY (CONTROL-P)
                  LET i = ARR_CURR()
                  CALL Busca_afiliados(g_reg[i].codven,
                                       g_reg[i].coduni_n1,
                                       g_reg[i].cod_tipo_prom,
                                       g_reg[i].fecha_corte,
                                       g_reg[i].vedo)
               ON KEY (CONTROL-B)
                  LET i = ARR_CURR()
                  CALL Busca_estado(g_reg[i].vedo)
               ON KEY (CONTROL-N)
                  LET i = ARR_CURR()
                  CALL Mostrar_nombre(g_reg[i].codven)
               ON KEY (INTERRUPT)
                  EXIT DISPLAY
            END DISPLAY
         ELSE
            ERROR "NO EXISTEN DATOS CON ESTAS CONDICIONES"
            SLEEP 3
            error ""
         END IF
         call Inicializa()
         CLEAR SCREEN
END FUNCTION

FUNCTION Busca_afiliados(vcodven,vcoduni_n1,vcod_tipo,vfec_corte,vestado)

   DEFINE registros2 INTEGER,
          monto      LIKE com_comis_detalle.monto_comision,
          pos        SMALLINT,
          parte1     CHAR(26),
          parte2     CHAR(10),
          parte3     CHAR(100),
          i          SMALLINT,
          vcodven    CHAR(10),
          vcoduni_n1 LIKE com_comis_detalle.coduni_n1,
          vcod_tipo  LIKE com_comis_detalle.cod_tipo_prom,
          vfec_corte LIKE com_comis_detalle.fecha_corte,
          vestado    LIKE com_comis_detalle.estado_comision


   OPEN WINDOW ventana_2 AT 9,2 WITH FORM "COMC0093"

   DISPLAY "                         CLIENTES DEL PROMOTOR                                " AT 2,1 ATTRIBUTE(REVERSE)


   DISPLAY "             [Ctrl-C] Salir                          [Enter] Nombre Cliente    " AT 04,1 ATTRIBUTE(REVERSE)

   DISPLAY "                                                                               " AT 11,1 ATTRIBUTE(REVERSE)


   LET registros2 = 0
   LET monto      = 0

   LET cla_sel2="SELECT a.n_folio,",
                       "a.nss,",
                       "a.tipo_solicitud,",
                       "a.fentcons,",
                       "a.fecha_corte,",
                       "a.salario_base_comis, ",
                       "a.num_sm, ",
                       "a.monto_comision, ",
                       "a.estado_comision ",
                "FROM   com_comis_detalle a ",
                "WHERE a.codven = '",vcodven CLIPPED,
                "' AND a.coduni_n1 = '",vcoduni_n1 CLIPPED,
                "' AND a.cod_tipo_prom = ",vcod_tipo CLIPPED,
                " AND a.fecha_corte = '",vfec_corte CLIPPED,
                "' AND a.estado_comision = ",vestado CLIPPED,
                " AND ",cla_where CLIPPED,
               " ORDER BY 2" CLIPPED

         ERROR "Buscando Informacion"

         PREPARE claexe_pend FROM cla_sel2
         DECLARE cursor_pend CURSOR FOR claexe_pend
         LET pos = 1
         FOREACH cursor_pend INTO g_reg3[pos].*
                 LET registros2 = registros2 + 1
                 LET monto = monto + g_reg3[pos].monto_comision
                 LET pos = pos + 1
                 IF pos >= 9000 THEN

                    ERROR "Sobrepaso la capacidad del arreglo"
                    EXIT FOREACH
                 END IF
         END FOREACH

         DISPLAY registros2 TO scr_1.registros2
         DISPLAY monto TO scr_1.monto

         ERROR ""

         CALL  SET_COUNT(pos-1)

         IF (pos-1) >= 1 THEN
            DISPLAY ARRAY g_reg3 TO scr_0.*
               ON KEY (CONTROL-M)
                  LET i = ARR_CURR()
                  CALL Mostrar_nombre2(g_reg3[i].nss)
               ON KEY (INTERRUPT)
                  EXIT DISPLAY
            END DISPLAY
         ELSE
            ERROR "NO EXISTEN DATOS CON ESTAS CONDICIONES"
            SLEEP 3
            error ""
         END IF
----         CLEAR SCREEN
         CLOSE WINDOW ventana_2
END FUNCTION

------------------------------------------------------

FUNCTION Busca_estado(vestado)
  DEFINE vestado    LIKE com_comis_detalle.estado_comision
  DEFINE pos        INTEGER

  DEFINE l_record   ARRAY[1000] OF RECORD
                codigo         SMALLINT,
                descripcion    CHAR(70)
         END RECORD
         DECLARE cursor_1 CURSOR FOR
         SELECT codigo,descripcion FROM tab_edo_comision
         WHERE codigo  = vestado
         ORDER BY 1
         LET pos = 1
         FOREACH cursor_1 INTO l_record[pos].*
                 LET pos = pos + 1
         END FOREACH
         IF (pos-1) >= 1 THEN
            CALL  SET_COUNT(pos-1)
         -- OPEN WINDOW ventana_2 AT 5,10 WITH FORM "COMC0095" ATTRIBUTE(BORDER)
                                  -- r,c
            OPEN WINDOW ventana_2 AT 5,3 WITH FORM "COMC0095" ATTRIBUTE( BORDER)


            DISPLAY " CONSULTA " AT 1,67 ATTRIBUTE(REVERSE,yellow)
            DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(REVERSE,YELLOW)
            DISPLAY "                    Descripcion de Estado                                        " AT 3,1 ATTRIBUTE(REVERSE,green)

            DISPLAY ARRAY l_record TO scr_9.*
                    ON KEY (INTERRUPT)
                       EXIT DISPLAY
            END DISPLAY
            CLOSE WINDOW ventana_2
         ELSE
            ERROR "ARCHIVO DE CANAL.... VACIO"
         END IF


END FUNCTION
FUNCTION Mostrar_nombre(vcodven)
   DEFINE vcodven    CHAR(10),
          pos        INTEGER,
          vlong      SMALLINT

   OPEN WINDOW ventana001 AT 09,2 WITH FORM "COMC0094"

   DISPLAY "                             NOMBRE DEL PROMOTOR                               " AT 2,1 ATTRIBUTE(REVERSE)

   DISPLAY "                                                              [Ctrl-C] Salir   " AT 04,1 ATTRIBUTE(REVERSE)

   DISPLAY "                                                                               " AT 11,1 ATTRIBUTE(REVERSE)

   IF LENGTH(vcodven) = 10 THEN
      LET cla_sel2="SELECT a.codven,",
                          "a.paterno,",
                          "a.materno,",
                          "a.nombres ",
                   "FROM   pro_mae_promotor a ",
         -- issa          "WHERE  a.cod_promotor  = ",vcodven
                   "WHERE  a.codven  = ",vcodven
   ELSE
      LET cla_sel2="SELECT '',",
                           "nombre_resp_uni,",
                           "'',",
                           "'' ",
                   "FROM   com_respon_unidad  ",
                   "WHERE  cod_resp_uni = ","'",vcodven,"'"
   END IF

   ERROR "Buscando Informacion"

   PREPARE claexe_cte FROM cla_sel2
   DECLARE cursor_cte CURSOR FOR claexe_cte
   LET pos = 1
   FOREACH cursor_cte INTO g_reg4[pos].*
      LET g_reg5[pos].codven = g_reg4[pos].codven
      LET g_reg5[pos].nombre = g_reg4[pos].paterno CLIPPED," ",
                               g_reg4[pos].materno CLIPPED," ",
                               g_reg4[pos].nombres CLIPPED
      LET pos = pos + 1
      IF pos >= 9000 THEN
         ERROR "Sobrepaso la capacidad del arreglo"
         EXIT FOREACH
      END IF
   END FOREACH

   ERROR ""

   CALL  SET_COUNT(pos-1)

   IF (pos-1) >= 1 THEN
       DISPLAY ARRAY g_reg5 TO scr_0.*
          ON KEY (INTERRUPT)
             EXIT DISPLAY
       END DISPLAY
   ELSE
      -- Buscamos el nombre del posible jefe
      LET vnom_jefe = ' '
      SELECT nombre_resp_uni
      INTO   vnom_jefe
      FROM   com_respon_unidad
      WHERE  cod_resp_uni = vcodven
      IF vnom_jefe IS NULL THEN
         ERROR "NO EXISTEN DATOS CON ESTAS CONDICIONES"
         SLEEP 3
         error ""
      ELSE
         LET g_reg5[pos].codven ='0000000000'
         LET g_reg5[pos].nombre = vnom_jefe
         DISPLAY ARRAY g_reg5 TO scr_0.*
            ON KEY (INTERRUPT)
               EXIT DISPLAY
         END DISPLAY

      END IF
   END IF
   CLOSE WINDOW ventana001
END FUNCTION
----------------------------------------------------------------------

FUNCTION Mostrar_nombre2(vnss)
   DEFINE vnss    CHAR(11),
          pos     INTEGER

   --OPEN WINDOW ventana001 AT 09,2 WITH FORM "COMC0095"
   OPEN WINDOW ventana001 AT 09,2 WITH FORM "COMC0094"

   DISPLAY "                             NOMBRE DEL CLIENTE                                " AT 2,1 ATTRIBUTE(REVERSE)

   DISPLAY "                                                              [Ctrl-C] Salir   " AT 04,1 ATTRIBUTE(REVERSE)

   DISPLAY "                                                                               " AT 11,1 ATTRIBUTE(REVERSE)  


   LET cla_sel2="SELECT a.n_seguro,",
                       "a.paterno,",
                       "a.materno,",
                       "a.nombres ",
                "FROM   afi_mae_afiliado a ",
                "WHERE  a.n_seguro  = '",vnss CLIPPED,
                "'"

         ERROR "Buscando Informacion"

         PREPARE claexe_cte2 FROM cla_sel2
         DECLARE cursor_cte2 CURSOR FOR claexe_cte2
         LET pos = 1
         FOREACH cursor_cte2 INTO g_reg4[pos].*
            LET g_reg5[pos].codven         = g_reg4[pos].codven

            LET g_reg5[pos].nombre = g_reg4[pos].paterno CLIPPED," ",
                                     g_reg4[pos].materno CLIPPED," ",
                                     g_reg4[pos].nombres CLIPPED
                 LET pos = pos + 1
                 IF pos >= 900 THEN
                    ERROR "Sobrepaso la capacidad del arreglo"
                    EXIT FOREACH
                 END IF
         END FOREACH


         ERROR ""

         CALL  SET_COUNT(pos-1)

         IF (pos-1) >= 1 THEN
            DISPLAY ARRAY g_reg5 TO scr_0.*
               ON KEY (INTERRUPT)
                  EXIT DISPLAY
            END DISPLAY
         ELSE
            ERROR "NO EXISTEN DATOS CON ESTAS CONDICIONES"
            SLEEP 3
            error ""
         END IF
----         CLEAR SCREEN
         CLOSE WINDOW ventana001
END FUNCTION
----------------------------------------------------------------------


