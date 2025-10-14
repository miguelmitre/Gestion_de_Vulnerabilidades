#*******************************************************************#
#Proyecto          => Sistema de Afores.( MEXICO )                  #
#Propietario       => E.F.P.                                        #
#Programa          => VOLB002                                       #
#Descripcion       => LIQUIDACION DE APORTACIONES VOLB002           #
#Sistema           => INT .                                         #
#Fecha             => 13 de Diciembre del 2005.                     #
#Por               => LAURA EUGENIA CORTES GUZMAN                   #
#Fecha Modif.      => 01-OCTUBRE-2007                               #
#Por               => STEFANIE DANIELA VERA PIÑA                    #
#CPL-1821          => Se corrige actualización fecha liquidacion    #
#CPL-1826          => FSR 22-12-2014                                # 
#                  => Se quita la rehabilitacion por traspaso 120   #
#*******************************************************************#
DATABASE  safre_af

GLOBALS

   DEFINE arr_liq ARRAY[500] OF RECORD
      folio      INTEGER,
      nss        CHAR(11),
      monto      DECIMAL(11,2),
      precio     DECIMAL(16,6),
      hora       CHAR(4),
      accion     DECIMAL(16,6),
      siefore    SMALLINT,
      aporte     CHAR(3),
      tienda     CHAR(04),
      f_pago     CHAR(01),
      seleccion  CHAR(1)
   END RECORD
   
   DEFINE arr_ri ARRAY[500] OF RECORD
      li_rowid   INTEGER
   END RECORD	

   DEFINE g_ruta RECORD LIKE seg_modulo.*

   DEFINE 
      fecha_liquidacion ,
      hoy               DATE

   DEFINE
      usuario           CHAR(08),
      enter             CHAR(1)
      
   DEFINE
      i                 ,
      ii                ,
      vfolio            INTEGER

   DEFINE
      ban               SMALLINT
     
END GLOBALS


MAIN
   DEFINE cuantos SMALLINT

   OPTIONS 
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST

   DEFER INTERRUPT

   CALL STARTLOG (FGL_GETENV("USER")||".VOLB002.log")

   SELECT *, USER
   INTO   g_ruta.*, usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "vol"

   LET INT_FLAG = FALSE

   LET fecha_liquidacion = TODAY

   LET hoy = TODAY
   LET ban = 0

   OPEN WINDOW ventana1 AT 2,2 WITH FORM "VOLB0021" ATTRIBUTE(BORDER)

   DISPLAY " <CONTROL-C> SALIR                                        <CONTROL-P> IMPRIME  " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " VOLB002               LIQUIDACION DE APORTACIONES                             " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY hoy USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

   INPUT BY NAME vfolio,fecha_liquidacion WITHOUT DEFAULTS

      AFTER FIELD vfolio
         IF vfolio IS NULL
         OR vfolio = 0 THEN
            ERROR " FOLIO INCORRECTO "
            NEXT FIELD vfolio
         END IF
         
         SELECT "a.x"
         FROM   int_det_voluntaria a
         WHERE  a.folio = vfolio
         AND    a.resul_operacion = "01"
         AND    a.estado = 1
         AND    a.tipo_aportacion IS NOT NULL     
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR " NO EXISTE INFORMACION DEL FOLIO "
            NEXT FIELD vfolio
         END IF

      AFTER FIELD fecha_liquidacion
         IF fecha_liquidacion IS NULL OR
            fecha_liquidacion = " " THEN
            ERROR " FECHA INCORRECTA "
            NEXT FIELD fecha_liquidacion
         END IF

         LET cuantos = 0

         SELECT count(*)
         INTO   cuantos
         FROM   glo_valor_accion a
         WHERE  a.fecha_valuacion = fecha_liquidacion

         IF cuantos < 3 THEN
            ERROR " FALTA INGRESAR ALGUN PRECIO DE ACCION "
            NEXT FIELD fecha_liquidacion
         END IF

         ERROR ""

         EXIT INPUT


      ON KEY(INTERRUPT, CONTROL-C)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      LET INT_FLAG = FALSE
      LET ban = 1
      DISPLAY "" AT 5,1
   ELSE

       CALL ejecuta_liquidacion()
       RETURNING ban
   END IF

   ERROR ""

   IF ban = 0 THEN
      PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR "
	    FOR enter
   ELSE
      PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR "
	    FOR enter
   END IF

   DISPLAY "" AT 5,1

   CLEAR FORM
   CLEAR SCREEN
   CLOSE WINDOW ventana1
END MAIN


FUNCTION ejecuta_liquidacion()
#-----------------------------

   DEFINE arr_band	ARRAY[500] OF SMALLINT

   DEFINE
      G_LISTA2     ,
      G_LISTA      CHAR(100),
      ejecuta      CHAR(600)

   DEFINE
      totalc       DECIMAL(16,6),
      totald       DECIMAL(16,6)

   DEFINE 
      totala       ,
      pos          INTEGER
 
   DEFINE
      arr_c        ,
      arr_l        ,
      arr_t        ,
      vmenos       SMALLINT
   

   INITIALIZE G_LISTA, G_LISTA2 TO NULL

   LET G_LISTA = g_ruta.ruta_listados CLIPPED,"/",
                 usuario CLIPPED,
                 ".A_LIQVOL_",
                 TODAY USING "ddmmyy"

   LET G_LISTA2= g_ruta.ruta_listados CLIPPED,"/",
                 usuario CLIPPED,
                 ".LIQVOL_",
                 TODAY USING "ddmmyy"

   ERROR " PROCESANDO LIQUIDACION "
   SLEEP 2

   LET pos = 1
   LET i   = 1
   LET ii  = 1

   START REPORT sal_vol TO G_LISTA
   DECLARE cur_sor1 CURSOR FOR
   SELECT  a.rowid,
           a.folio,
           a.nss,
           a.monto_neto  ,
           f.precio_del_dia,
           a.hora,
           (a.monto_neto / f.precio_del_dia),
           m.codigo_siefore,
           a.tipo_aportacion,
           a.tienda_numero,     
           a.forma_pago
   FROM    int_det_voluntaria a ,
           glo_valor_accion f,
           cta_regimen m
   WHERE   a.folio = vfolio 
   AND     a.resul_operacion = "01"
   AND     a.estado = 1
   AND     a.tipo_aportacion IS NOT NULL
   AND     a.nss = m.nss
   AND     m.codigo_siefore = f.codigo_siefore
   AND     f.fecha_valuacion = fecha_liquidacion  
   AND     m.subcuenta= decode (a.tipo_aportacion,
                               'V',10,
                               'C',12,
                               'L',16)
   UNION 
   SELECT a.rowid,
          a.folio,              #-- REINTEGRO --#
          a.nss,
          a.monto_neto,
          0,
          a.hora,
          0,
          0,
          a.tipo_aportacion,
          a.tienda_numero,
          a.forma_pago
   FROM   int_det_voluntaria a 
   WHERE  a.folio = vfolio
   AND    a.resul_operacion = "01"
   AND    a.estado = 1                               
   AND     a.tipo_aportacion = "R"

   FOREACH cur_sor1 INTO arr_ri[pos].* ,arr_liq[pos].*#CPL-1821
      IF arr_liq[pos].aporte = "R" THEN #-- REINTEGRO --#

         LET arr_liq[pos].aporte = "RCV" #CPL-1821

         SELECT a.codigo_siefore,
                a.precio_del_dia
         INTO   arr_liq[pos].siefore,
                arr_liq[pos].precio
         FROM   glo_valor_accion a,
                cta_regimen b
         WHERE  b.nss = arr_liq[pos].nss
         AND    b.codigo_siefore = a.codigo_siefore
         AND    a.fecha_valuacion = fecha_liquidacion
         AND    b.subcuenta IN (1,2,5,6,9)
         GROUP BY 1,2
         
         LET arr_liq[pos].accion = arr_liq[pos].monto / arr_liq[pos].precio

      END IF
      
      OUTPUT TO REPORT sal_vol(arr_liq[pos].*,fecha_liquidacion)
      
      LET pos = pos + 1

   END FOREACH
   
   FINISH REPORT sal_vol

   LET ban = 0
   
   CALL SET_COUNT(pos-1)
   
   IF pos <= 1 THEN
      ERROR ""
      PROMPT " NO EXISTE INFORMACION DEL FOLIO < ENTER > PARA SALIR "
      FOR CHAR enter
      LET ban = 1
      RETURN ban
   END IF
   
	 FOR i = 1 TO pos 
		  LET arr_band[i] = FALSE
	 END FOR

   LET totala = 0
   LET totalc = 0
   LET totald = 0

   START REPORT sal_vol_dos TO G_LISTA2
   INPUT ARRAY arr_liq WITHOUT DEFAULTS FROM scr_liquida.*
      BEFORE FIELD seleccion
			LET arr_c = ARR_CURR()
			LET arr_l = SCR_LINE()

      AFTER FIELD seleccion
         LET arr_c = ARR_CURR()
         LET arr_l = SCR_LINE()
         
         IF arr_liq[arr_c].seleccion = "X" AND
            arr_liq[arr_c].folio IS NULL THEN
            	
            LET arr_liq[arr_c].seleccion = " "
            DISPLAY arr_liq[arr_c].seleccion TO seleccion[arr_l]
         END IF

			   # Quitan Marca X, resta acumulados y apaga bandera de Marcado
			   IF arr_liq[arr_c].seleccion IS NULL AND arr_band[arr_c] THEN
				    LET totala = totala - 1 
				    LET totalc = totalc - arr_liq[arr_c].monto
				    LET totald = totald - arr_liq[arr_c].accion
				    LET arr_band[arr_c] = FALSE
				    
            DISPLAY BY NAME totala, totalc, totald
			   END IF

			   # Pone Bandera de Marcado con X y lo acumula
         IF arr_liq[arr_c].seleccion = "X" AND NOT arr_band[arr_c] THEN
				    LET totala = totala + 1 
    				LET totalc = totalc + arr_liq[arr_c].monto  
		    		LET totald = totald + arr_liq[arr_c].accion
				    LET arr_band[arr_c] = TRUE
            DISPLAY BY NAME totala, totalc, totald
         END IF


      ON KEY(CONTROL-P)
         LET ejecuta = "lp ",G_LISTA CLIPPED
         RUN ejecuta

      ON KEY (ESC)
         DISPLAY BY NAME totala,totalc,totald

         PROMPT " ES CORRECTA LA SUMA A LIQUIDAR [S/N] ? "
         FOR enter
         	
         DISPLAY BY NAME totala,totalc,totald

         IF enter MATCHES "[Ss]" THEN
            PROMPT " DESEA GENERAR EL PROCESO S/N ? " 
            FOR CHAR enter

            IF enter MATCHES "[Ss]" THEN
               ERROR " PROCESANDO INFORMACION .... ESPERE UN MOMENTO " 
               ATTRIBUTE(BOLD)

               LET vmenos = pos

               FOR i = 1 to vmenos
                  IF arr_liq[i].seleccion = "X" THEN
                      OUTPUT TO REPORT sal_vol_dos(arr_liq[i].*,
                                                   fecha_liquidacion,
                                                   totalc,
                                                   totald)
                      CALL liquidacion(arr_liq[i].*,fecha_liquidacion, arr_ri[i].*)
                      
                      CALL rehabilita(arr_liq[i].nss,arr_liq[i].monto)

                  END IF
               END FOR

               FINISH REPORT sal_vol_dos

               LET ejecuta = "lp ",G_LISTA2 CLIPPED
               RUN ejecuta

               ERROR ""
               PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR "
               FOR CHAR enter
               LET ban = 0
            ELSE
               ERROR " PROCESO CANCELADO...<ENTER> PARA SALIR "
               SLEEP 2
               ERROR ""
               LET ban = 1
            END IF
            EXIT INPUT
         END IF

         FOR i = 1 TO pos 
            LET arr_liq[i].seleccion = NULL
            LET totala = 0
            LET totalc = 0
            LET totald = 0

            DISPLAY  arr_liq[i].seleccion TO scr_liquida[i].seleccion

            DISPLAY BY NAME totala,totalc,totald
         END FOR

      ON KEY (INTERRUPT,CONTROL-C)
         ERROR " PROCESO CANCELADO...<ENTER> PARA SALIR "
         SLEEP 2
         ERROR ""
         EXIT INPUT
         LET ban = 1
   END INPUT

   RETURN ban
END FUNCTION


FUNCTION rehabilita(vnss,vmonto)
#------------------------------
   DEFINE #loc #cha
       vdesmarca     CHAR(100),
       vnss          CHAR(11),
       vusuario      CHAR(08)

   DEFINE #loc #decimal
       vmonto        DECIMAL(16,6)

   DEFINE #loc #smallint
       vestado_marca ,
       vmarca_entra  ,
       vmarca_causa  ,
       vmarca_ret    SMALLINT

   DEFINE
       vcorrelativo  LIKE cta_act_marca.correlativo

   SELECT COUNT(*),
          correlativo,
          marca_cod
   INTO   vmarca_ret,
          vcorrelativo,
          vmarca_entra
   FROM   cta_act_marca
   WHERE  nss = vnss
   AND    marca_cod =140 #CPL-1826 antes (120,140)
   GROUP BY 2,3

   IF vmarca_ret = 1 THEN
      LET vmarca_causa = 102
      LET vestado_marca = 0

      SELECT USER
      INTO   vusuario
      FROM   tab_afore_local

      LET vdesmarca = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"

      PREPARE marcaje_ret FROM vdesmarca

      EXECUTE marcaje_ret USING vnss         ,# nss
                                vmarca_entra ,# marca_entra
                                vcorrelativo ,# correlativo
                                vestado_marca,# estado_marca
                                vmarca_causa ,# marca_causa
                                vusuario      # usuario


      INSERT INTO cta_rehabilitada
      VALUES (
              vnss    ,#nss
              0       ,#monto_retiro
              0       ,#monto_cesantia
              vmonto  ,#monto_voluntaria
              0       ,#monto_vivienda97
              0       ,#monto_cuota_soc
              0       ,#monto_sar
              0       ,#monto_vivienda92
              HOY     ,#fecha_rehabilita
              102     ,#marca_cod
              HOY     ,#fecha_actualiza
              0       ,#estado
              USER     #usuario
             )
   END IF

END FUNCTION


FUNCTION liquidacion(l_reg,f_liquida,li_rowid)
#------------------------------------

   DEFINE l_reg RECORD
      folio      INTEGER,
      nss        CHAR(11),
      monto      DECIMAL(11,2),
      precio     DECIMAL(16,6),
      hora       CHAR(4),
      accion     DECIMAL(16,6),
      siefore    SMALLINT,
      aporte     CHAR(1),
      tienda     CHAR(04),
      f_pago     CHAR(01),
      seleccion  CHAR(1)
   END RECORD
   
   DEFINE li_rowid INTEGER

   DEFINE
      x_id_aportante   CHAR(08),
      cla_spl          CHAR(500)

   DEFINE
      f_liquida        DATE
      
   DEFINE   
      vmonto_aporte    DECIMAL(11,2),
      vmonto_pesos     ,
      vmonto_pes_rcv   DECIMAL(16,6)      
   
   DEFINE
      consecutivo      ,
      x_subcuenta      SMALLINT

#codigo nuevo {
   #Recibe los datos de la consulta   
   DEFINE lr_liquida RECORD
          subcuenta  LIKE dis_cuenta.subcuenta,
          monto      LIKE dis_cuenta.monto_en_pesos
        END RECORD
   #Almacena los datos de la subcuenta y el monto que tiene
   DEFINE la_liquida ARRAY[10] OF RECORD
          subcuenta  LIKE dis_cuenta.subcuenta,
          monto      LIKE dis_cuenta.monto_en_pesos
          END RECORD
   
   DEFINE ls_pos         SMALLINT
   DEFINE ls_contador    SMALLINT
   DEFINE ld_acumulado   LIKE dis_cuenta.monto_en_pesos #acumulado para la liquidacion
   
#} codigo nuevo

   LET consecutivo = consecutivo + 1
   LET x_subcuenta = 0
   LET f_liquida   = HOY
   INITIALIZE cla_spl TO NULL
   LET ld_acumulado = 0
   LET ls_pos       = 1


   IF l_reg.aporte = "R" THEN  #-- REINTEGRO --#
      
      LET vmonto_aporte = l_reg.monto

      SELECT NVL(SUM(monto_en_acciones),0) * l_reg.precio
      INTO   vmonto_pes_rcv
      FROM   dis_cuenta
      WHERE  nss = l_reg.nss
      AND    subcuenta IN (1,2,5,6,9)


      IF vmonto_pes_rcv = 0 THEN

         DECLARE cur_2 CURSOR FOR
         SELECT subcuenta
         FROM   cta_regimen
         WHERE  nss = l_reg.nss
         AND    subcuenta IN (1,2,5,6)
         
         
         FOREACH cur_2 INTO x_subcuenta
         
            CASE x_subcuenta
               WHEN 1
                  LET  l_reg.monto = (vmonto_aporte * 17.45 ) / 100
               WHEN 2
                  LET  l_reg.monto = (vmonto_aporte * 37.30 ) / 100
               WHEN 5
                  LET  l_reg.monto = (vmonto_aporte * 43.29 ) / 100
               WHEN 6
                  LET  l_reg.monto = (vmonto_aporte * 1.96 ) / 100
            END CASE
            
            LET l_reg.accion = l_reg.monto / l_reg.precio
         
            CALL agrega_liquidacion(x_subcuenta,l_reg.*,consecutivo,f_liquida)
            
         END FOREACH
      ELSE
         
         SELECT MAX(subcuenta)
         INTO ls_contador
         FROM   dis_cuenta
         WHERE  nss = l_reg.nss
         AND    subcuenta IN (1,2,5,6,9)

         DECLARE cur_3 CURSOR FOR
         SELECT subcuenta,
                NVL(SUM(monto_en_acciones),0) * l_reg.precio
         FROM   dis_cuenta
         WHERE  nss = l_reg.nss
         AND    subcuenta IN (1,2,5,6,9)
         GROUP BY 1
         ORDER BY subcuenta
         
         FOREACH cur_3 INTO lr_liquida.*
         LET ls_pos = lr_liquida.subcuenta
         #se llena el arreglo
         LET la_liquida[ls_pos].* = lr_liquida.*
         
         #Cuando el monto de la subcuenta sea diferente de cero
         IF la_liquida[ls_pos].monto <> 0 THEN
           
           #Evaluar si es la ultima siefore
           IF la_liquida[ls_pos].subcuenta = ls_contador THEN
             #se resta el acumulado al monto aportado
             LET l_reg.monto  = vmonto_aporte - ld_acumulado
             LET l_reg.accion = l_reg.monto / l_reg.precio
             CALL agrega_liquidacion(la_liquida[ls_pos].subcuenta           ,
                                                        l_reg.*,consecutivo ,
                                                        f_liquida            )
           ELSE
             #Se hace de manera tradicional
             LET l_reg.monto = (la_liquida[ls_pos].monto * vmonto_aporte) / vmonto_pes_rcv
             LET l_reg.accion = l_reg.monto / l_reg.precio
             LET ld_acumulado = ld_acumulado + l_reg.monto
             CALL agrega_liquidacion(la_liquida[ls_pos].subcuenta           ,
                                                        l_reg.*,consecutivo ,
                                                        f_liquida            )
           END IF
           
         END IF
         
         #En éste punto, ls_contador sabe cuantas subcuentas se liquidarán
         #y ls_pos sabe cual es la ultima subcuenta a liquidar
         {
            LET l_reg.monto = (vmonto_pesos * vmonto_aporte) / vmonto_pes_rcv
         
            LET l_reg.accion = l_reg.monto / l_reg.precio
         
            CALL agrega_liquidacion(x_subcuenta,l_reg.*,consecutivo,f_liquida)
         }
         END FOREACH
        END IF
        

      INSERT INTO ret_notifica_devol
      SELECT nss,
             num_resolucion,
             fecha_retiro,
             semanas_reintegro,
             mto_reintegro,
             "",                  #-- fecha_pago_reint --#
             "",                  #-- resultado_operacion --#
             "",                  #-- diag_proceso --#
             "",                  #-- fecha_notifica_pago --#
             "",                  #-- fecha_diag_pago --#
             50
      FROM   ret_mto_devol
      WHERE  nss = l_reg.nss
      AND    mto_reintegro = vmonto_aporte
      AND    estado_solicitud = 50   #-- RECIBIDO --#

      UPDATE ret_notifica_devol
      SET    fecha_pago_reint = f_liquida,
             estado_solicitud = 80
      WHERE  nss = l_reg.nss
      AND    mto_reintegro = vmonto_aporte
      AND    estado_solicitud = 50

      UPDATE ret_mto_devol
      SET    estado_solicitud = 80   #-- LIQUIDADO --#
      WHERE  nss = l_reg.nss
      AND    mto_reintegro = vmonto_aporte
      AND    estado_solicitud = 50

   ELSE
      CASE l_reg.aporte
         WHEN "V"
            LET x_subcuenta = 10
         WHEN "C"
            LET x_subcuenta = 12
         WHEN "L"
            LET x_subcuenta = 16
      END CASE
      
      CALL agrega_liquidacion(x_subcuenta,l_reg.*,consecutivo,f_liquida)
   END IF
   
   UPDATE int_det_voluntaria
   SET    int_det_voluntaria.estado = 2,
          int_det_voluntaria.fecha_liquida = f_liquida
   WHERE  int_det_voluntaria.folio = l_reg.folio
   AND    int_det_voluntaria.nss = l_reg.nss
   #AND    int_det_voluntaria.monto_neto = l_reg.monto
   AND    int_det_voluntaria.tipo_aportacion = l_reg.aporte
   AND    int_det_voluntaria.estado = 1
 	 AND    int_det_voluntaria.hora = l_reg.hora
 	 AND    rowid = li_rowid #CPL-1821
 	  	 
END FUNCTION


FUNCTION agrega_liquidacion(vsubcuenta,l_reg,vconsecutivo,f_liquida)
#-------------------------------------------------------------------

   DEFINE l_reg  RECORD
      folio      INTEGER,
      nss        CHAR(11),
      monto      DECIMAL(11,2),
      precio     DECIMAL(16,6),
      hora       CHAR(4),
      accion     DECIMAL(16,6),
      siefore    SMALLINT,
      aporte     CHAR(1),
      tienda     CHAR(04),
      f_pago     CHAR(01),
      seleccion  CHAR(1)
   END RECORD

   DEFINE
      cla_spl           CHAR(500),
      x_id_aportante    CHAR(08)

   DEFINE
      f_liquida         DATE

   DEFINE
      vconsecutivo      ,
      vsubcuenta        ,
      vcrea_saldo       ,
      vtipo_movimiento  SMALLINT

   IF vsubcuenta = 1 OR
      vsubcuenta = 2 OR
      vsubcuenta = 5 OR
      vsubcuenta = 6 OR
      vsubcuenta = 9 THEN

      LET vtipo_movimiento = 83
      LET x_id_aportante = "REINTEGRO"
      LET vcrea_saldo = 0
   ELSE
      LET vtipo_movimiento = 1
      LET x_id_aportante = "VE-",l_reg.tienda CLIPPED
      LET vcrea_saldo = 1
   END IF

   INITIALIZE cla_spl TO NULL

   INSERT INTO dis_cuenta
   VALUES( vtipo_movimiento,       --tipo_movimiento
           vsubcuenta,             --subcuenta
           l_reg.siefore,          --siefore
           l_reg.folio,            --folio
           vconsecutivo,           --consecutivo_lote
           l_reg.nss,              --nss
           " ",                    --curp
           " ",                    --folio_sua
           f_liquida,              --fecha_pago
           f_liquida,              --fecha_valor
           f_liquida,              --fecha_conversion
           l_reg.monto,            --monto_en_pesos
           l_reg.accion,           --monto_en_acciones
           l_reg.precio,           --precio_accion
           0,                      --dias_cotizados
           l_reg.tienda,           --sucursal
           x_id_aportante,         --id_aportante
           5,                      --estado
           HOY,                    --fecha_proceso
           USER,                   --usuario
           HOY,                    --fecha_archivo
           0 )                     --etiqueta

   IF vcrea_saldo = 1 THEN
      LET cla_spl = "EXECUTE PROCEDURE crea_saldo_vol(",
                     l_reg.folio, ",",
                     '"', l_reg.nss, '"', ",",
                     l_reg.siefore, ",",
                     vsubcuenta, ",",
                     '"', f_liquida, '"', ",",
                     '"', f_liquida, '"', ",",
                     l_reg.monto, ",",
                     l_reg.accion, ",",
                     '"', usuario, '"', ")"
      
      LET cla_spl = cla_spl CLIPPED
      PREPARE claexe FROM cla_spl
      EXECUTE claexe
   END IF

END FUNCTION


REPORT sal_vol(l_reg,f_liquida)
#------------------------------

   DEFINE l_reg RECORD
      folio      INTEGER,
      nss        CHAR(11),
      monto      DECIMAL(11,2),
      precio     DECIMAL(16,6),
      hora       CHAR(4),
      accion     DECIMAL(16,6),
      siefore    SMALLINT,
      aporte     CHAR(1),
      tienda     CHAR(04),
      f_pago     CHAR(01),
      seleccion  CHAR(1)
   END RECORD,

      f_liquida      DATE,
      tot_monto      DECIMAL(11,2),
      tot_precio     DECIMAL(16,6),
      tot_accion     DECIMAL(16,6),
      des_tp         CHAR(14) 

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
     
   FORMAT
      PAGE HEADER
         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
         PRINT COLUMN 38," REPORTE DE APORTACIONES POR LIQUIDAR ",
               COLUMN 111,TODAY USING "dd/mm/yyyy"
         PRINT COLUMN 038,"       FECHA LIQUIDACION ",
                          f_liquida USING "DD/MM/YYYY",
               COLUMN 111,"   VOLB002"
         PRINT COLUMN 01,"----------------------------------------",
               COLUMN 41,"----------------------------------------",
               COLUMN 81,"----------------------------------------"
         PRINT COLUMN 01,"F O L I O ",
               COLUMN 15,"N. S. S. ",
               COLUMN 30,"M O N T O ",
               COLUMN 49,"PRECIO ACCION",
               COLUMN 69,"A C C I O N E S ",
               COLUMN 89,"SIEFORE",
               COLUMN 100,"APORTACION",
               COLUMN 113,"FORMA PAGO"

         PRINT COLUMN 01,"----------------------------------------",
               COLUMN 41,"----------------------------------------",
               COLUMN 81,"----------------------------------------"
        
         SKIP 1 LINE

      ON EVERY ROW
         CASE l_reg.aporte
            WHEN "V"  LET des_tp = "VOLUNTARIA    "
            WHEN "C"  LET des_tp = "COMPLEMENTARIA"
            WHEN "L"  LET des_tp = "LARGO PLAZO   "
         END CASE

         PRINT COLUMN 01,l_reg.folio USING "#########&",
               COLUMN 15,l_reg.nss,
               COLUMN 30,l_reg.monto USING "#######&.&&",
               COLUMN 49,l_reg.precio USING "######&.&&&&&&",
               COLUMN 69,l_reg.accion USING "######&.&&&&&&",
               COLUMN 92,l_reg.siefore,
               COLUMN 100,des_tp,
               COLUMN 118,l_reg.f_pago

         LET tot_monto  = tot_monto  + l_reg.monto
         LET tot_precio = tot_precio + l_reg.precio
         LET tot_accion = tot_accion + l_reg.accion


      ON LAST ROW
         SKIP 4 LINE
         
         PRINT COLUMN 01,"----------------------------------------",
               COLUMN 41,"----------------------------------------",
               COLUMN 81,"----------------------------------------"
         PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<<<<",
               COLUMN 30,SUM(l_reg.monto) USING "#######&.&&",
               COLUMN 49,tot_precio USING "######&.&&&&&&",
               COLUMN 69,SUM(l_reg.accion) USING "######&.&&&&&&"

END REPORT


REPORT sal_vol_dos(l_reg2,f_liquida, tot_monto,tot_accion)
#---------------------------------------------------------
   
   DEFINE l_reg2 RECORD
      folio      INTEGER,
      nss        CHAR(11),
      monto      DECIMAL(11,2),
      precio     DECIMAL(16,6),
      hora       CHAR(4),
      accion     DECIMAL(16,6),
      siefore    SMALLINT,
      aporte     CHAR(1),
      tienda     CHAR(04),
      f_pago     CHAR(01),
      seleccion  CHAR(1)
    END RECORD,
   
      f_liquida      DATE,
      tot_monto      DECIMAL(11,2),
      tot_accion     DECIMAL(16,6),
      tot_precio     DECIMAL(16,6),
      des_tp         CHAR(14) 

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60

   FORMAT
      PAGE HEADER
         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
         PRINT COLUMN 38," REPORTE DE APORTACIONES POR LIQUIDAR ",
               COLUMN 111,TODAY USING "dd/mm/yyyy"
         PRINT COLUMN 038,"       FECHA LIQUIDACION ",
                          f_liquida USING "DD/MM/YYYY",
               COLUMN 111,"   VOLB002"
         PRINT COLUMN 01,"----------------------------------------",
               COLUMN 41,"----------------------------------------",
               COLUMN 81,"----------------------------------------"
         PRINT COLUMN 01,"F O L I O ",
               COLUMN 15,"N. S. S. ",
               COLUMN 30,"M O N T O ",
               COLUMN 49,"PRECIO ACCION",
               COLUMN 69,"A C C I O N E S ",
               COLUMN 89,"SIEFORE",
               COLUMN 100,"APORTACION"

         PRINT COLUMN 01,"----------------------------------------",
               COLUMN 41,"----------------------------------------",
               COLUMN 81,"----------------------------------------"
         SKIP 1 LINE

      
      ON EVERY ROW

         CASE l_reg2.aporte
            WHEN "V"  LET des_tp = "VOLUNTARIA    "
            WHEN "C"  LET des_tp = "COMPLEMENTARIA"
            WHEN "L"  LET des_tp = "LARGO PLAZO   "
         END CASE

         PRINT COLUMN 01,l_reg2.folio USING "#########&",
               COLUMN 15,l_reg2.nss,
               COLUMN 30,l_reg2.monto USING "#######&.&&",
               COLUMN 49,l_reg2.precio USING "######&.&&&&&&",
               COLUMN 69,l_reg2.accion USING "######&.&&&&&&",
               COLUMN 92,l_reg2.siefore,
               COLUMN 100,des_tp

         LET tot_monto = tot_monto + l_reg2.monto
         LET tot_precio = tot_precio + l_reg2.precio
         LET tot_accion = tot_accion + l_reg2.accion


      ON LAST ROW
         SKIP 4 LINE
         
         PRINT COLUMN 01,"----------------------------------------",
               COLUMN 41,"----------------------------------------",
               COLUMN 81,"----------------------------------------"
         PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<<<<",
               COLUMN 30,SUM(l_reg2.monto) USING "#######&.&&",
               COLUMN 69,SUM(l_reg2.accion) USING "######&.&&&&&&"

END REPORT
