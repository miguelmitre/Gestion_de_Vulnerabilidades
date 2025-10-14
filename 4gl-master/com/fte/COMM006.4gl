################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                            #
#Owner             => E.F.P.                                                   #
#Programa COMM006  => MANTENEDOR DE TABLA DE PUESTOS                           #
#Sistema           => COM.                                                     #
#By                => GERARDO ALFONSO VEGA PAREDES.                            #
#Fecha             => 25 febrero 1999.                                         #
#Fecha             => 16 enero 2001.                                           #
################################################################################
DATABASE safre_af

GLOBALS

   DEFINE aux_pausa CHAR(1)

   DEFINE g_reg ARRAY[100] OF RECORD 
      cod_puesto        LIKE tab_puesto.cod_puesto,
      desc_puesto       LIKE tab_puesto.desc_puesto,
      cod_esq_comision  LIKE tab_puesto.cod_esq_comision,
      desc_esq_comision LIKE com_esq_comis.desc_esq_comision,
      cod_esq_bono      LIKE tab_puesto.cod_esq_bono,
      desc_esq_bono     LIKE com_esq_bono.desc_esq_bono
--      cod_esq_ispt      LIKE tab_puesto.cod_esq_ispt,
--      desc_esq_ispt     LIKE com_esq_ispt.desc_esq_ispt
   END RECORD

   DEFINE g_meta RECORD                            
      meta_afi1  LIKE com_tipo_promotor.meta_afi1, 
      meta_afi2  LIKE com_tipo_promotor.meta_afi2, 
      meta_afi3  LIKE com_tipo_promotor.meta_afi3, 
      meta_tra1  LIKE com_tipo_promotor.meta_tra1, 
      factualiza LIKE com_tipo_promotor.factualiza,
      usuario    LIKE com_tipo_promotor.usuario    
   END RECORD                                      

   DEFINE g_meta2 RECORD                         
      meta_afi1 LIKE com_tipo_promotor.meta_afi1,
      meta_afi2 LIKE com_tipo_promotor.meta_afi2, 
      meta_afi3 LIKE com_tipo_promotor.meta_afi3, 
      meta_tra1 LIKE com_tipo_promotor.meta_tra1, 
      usuario   LIKE com_tipo_promotor.usuario   
   END RECORD                                    

   DEFINE hoy DATE   

   DEFINE g_com_parametro RECORD LIKE com_parametro.*

   DEFINE arr_c,scr_l SMALLINT,
          opc         CHAR(01),
          vhora       CHAR(08),
          usuario     CHAR(08)

END GLOBALS

MAIN
   OPTIONS 
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY control-o

   DEFER INTERRUPT

   LET hoy = DATE

   OPEN WINDOW ventana_1 AT 3,2 WITH FORM "COMM0061" ATTRIBUTE( BORDER)
   DISPLAY " COMM006             MANTENIMIENTO TABLA DE PUESTOS                                " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY hoy USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

   MENU "PUESTOS"
      COMMAND "Agrega" "Agrega Puestos"
         CALL Inicializa()
         CALL Agrega()
         CALL Inicializa()
      COMMAND "Consulta" "Consulta Puestos"
         CALL Inicializa()
         CALL Consulta()
         CALL Inicializa()
      COMMAND "Modifica" "Modifica Puestos"
         CALL Inicializa()
         CALL Modifica()
         CALL Inicializa()
      COMMAND "Elimina" "Elimina Puestos"
         CALL Inicializa()
         CALL Elimina()
         CALL Inicializa()
      COMMAND "Salir" "Salir del Programa"
        EXIT MENU
   END MENU

   CLOSE WINDOW ventana_1
END MAIN

FUNCTION Inicializa()
   DEFINE i SMALLINT

   INITIALIZE g_reg     TO NULL
   INITIALIZE g_meta.*  TO NULL
   INITIALIZE g_meta2.* TO NULL

   FOR i = 1 TO 14
      DISPLAY g_reg[i].* TO scr_1[i].*
   END FOR

   SELECT *,                
          user              
   INTO   g_com_parametro.*,
          usuario           
   FROM   com_parametro     

   LET g_meta.meta_afi1  = 0      
   LET g_meta.meta_afi2  = 0      
   LET g_meta.meta_afi3  = 0      
   LET g_meta.meta_tra1  = 0      
   LET g_meta.factualiza = hoy    
   LET g_meta.usuario    = usuario

END FUNCTION

FUNCTION Agrega()
   DEFINE i SMALLINT

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " AGREGA " AT 1,71 ATTRIBUTE(REVERSE,BOLD)
   DISPLAY " [Esc] Agrega            [Ctrl-b] Metas            [Ctrl-c] Salir" AT 1,1 ATTRIBUTE(BOLD)
   INPUT ARRAY g_reg FROM scr_1.*

      BEFORE FIELD cod_puesto
         LET arr_c = ARR_CURR()
         LET scr_l = SCR_LINE()

      AFTER FIELD cod_puesto
         IF g_reg[arr_c].cod_puesto IS NULL THEN
            ERROR "Puesto NO puede ser NULO"
            NEXT FIELD cod_puesto
         END IF

         FOR i = 1 TO arr_c-1
            IF g_reg[arr_c].cod_puesto=g_reg[i].cod_puesto THEN
               ERROR "Puesto YA Ingresado en el Arreglo"
               LET g_reg[arr_c].cod_puesto = NULL
               DISPLAY g_reg[arr_c].cod_puesto TO
                       scr_1[scr_l].cod_puesto
               NEXT FIELD cod_puesto
            END IF
         END FOR

         SELECT "X" 
         FROM   tab_puesto
         WHERE  cod_puesto = g_reg[arr_c].cod_puesto
         IF STATUS <> NOTFOUND THEN
            ERROR "Puesto YA EXISTE"
            LET g_reg[arr_c].cod_puesto = NULL
            DISPLAY g_reg[arr_c].cod_puesto TO
                    scr_1[scr_l].cod_puesto
            NEXT FIELD cod_puesto
         END IF

      BEFORE FIELD desc_puesto
         LET arr_c = ARR_CURR()
         LET scr_l = SCR_LINE()

      AFTER FIELD desc_puesto
         IF g_reg[arr_c].desc_puesto IS NULL THEN
            ERROR "Descripcion Puesto NO puede ser NULO"
            NEXT FIELD desc_puesto
         END IF

      BEFORE FIELD cod_esq_comision
         LET arr_c = ARR_CURR()
         LET scr_l = SCR_LINE()

      AFTER FIELD cod_esq_comision
         IF g_reg[arr_c].cod_esq_comision IS NULL THEN
            CALL Despliega_cuadro_comision() 
                 Returning g_reg[arr_c].cod_esq_comision,
                           g_reg[arr_c].desc_esq_comision
         ELSE
            SELECT desc_esq_comision 
            INTO   g_reg[arr_c].desc_esq_comision
            FROM   com_esq_comis
            WHERE  cod_esq_comision = g_reg[arr_c].cod_esq_comision

            DISPLAY g_reg[arr_c].desc_esq_comision TO
                    scr_1[scr_l].desc_esq_comision
         END IF

         WHILE TRUE
            IF g_reg[arr_c].cod_esq_comision IS NULL THEN
               CALL Despliega_cuadro_comision() 
                    Returning g_reg[arr_c].cod_esq_comision,
                              g_reg[arr_c].desc_esq_comision
            ELSE 
               EXIT WHILE
            END IF
         END WHILE

         SELECT "X" 
         FROM   com_esq_comis
         WHERE  cod_esq_comision = g_reg[arr_c].cod_esq_comision
         IF STATUS = NOTFOUND THEN
            CALL Despliega_cuadro_comision() 
                 Returning g_reg[arr_c].cod_esq_comision,
                           g_reg[arr_c].desc_esq_comision
         END IF

      BEFORE FIELD cod_esq_bono
         LET arr_c = ARR_CURR()
         LET scr_l = SCR_LINE()

      AFTER FIELD cod_esq_bono
         IF g_reg[arr_c].cod_esq_bono IS NULL THEN
            CALL Despliega_cuadro_bono() 
                 Returning g_reg[arr_c].cod_esq_bono,
                           g_reg[arr_c].desc_esq_bono
         ELSE
            SELECT desc_esq_bono 
            INTO   g_reg[arr_c].desc_esq_bono
            FROM   com_esq_bono
            WHERE  cod_esq_bono = g_reg[arr_c].cod_esq_bono

            DISPLAY g_reg[arr_c].desc_esq_bono TO
                    scr_1[scr_l].desc_esq_bono
         END IF

         WHILE TRUE
            IF g_reg[arr_c].cod_esq_bono IS NULL THEN
               CALL Despliega_cuadro_bono() 
                    Returning g_reg[arr_c].cod_esq_bono,
                              g_reg[arr_c].desc_esq_bono
            ELSE 
               EXIT WHILE
            END IF
         END WHILE

         SELECT "X"
         FROM  com_esq_bono
         WHERE cod_esq_bono = g_reg[arr_c].cod_esq_bono

         IF STATUS = NOTFOUND THEN
            CALL Despliega_cuadro_bono() 
                 Returning g_reg[arr_c].cod_esq_bono,
                           g_reg[arr_c].desc_esq_bono
         END IF

{
      BEFORE FIELD cod_esq_ispt
         LET arr_c = ARR_CURR()
         LET scr_l = SCR_LINE()

      AFTER FIELD cod_esq_ispt
         IF g_reg[arr_c].cod_esq_ispt IS NULL THEN
            CALL Despliega_com_esq_ispt() 
                 Returning g_reg[arr_c].cod_esq_ispt,
                           g_reg[arr_c].desc_esq_ispt
         ELSE
            SELECT desc_esq_ispt 
            INTO   g_reg[arr_c].desc_esq_ispt
            FROM   com_esq_ispt
            WHERE  cod_esq_ispt = g_reg[arr_c].cod_esq_ispt

            DISPLAY g_reg[arr_c].desc_esq_ispt 
                 TO scr_1[scr_l].desc_esq_ispt
         END IF

         WHILE TRUE
            IF g_reg[arr_c].cod_esq_ispt IS NULL THEN
               CALL Despliega_com_esq_ispt() 
               Returning g_reg[arr_c].cod_esq_ispt,
                         g_reg[arr_c].desc_esq_ispt
            ELSE 
               EXIT WHILE
            END IF
         END WHILE

         SELECT "X" 
         FROM   com_esq_ispt
         WHERE  cod_esq_ispt = g_reg[arr_c].cod_esq_ispt
         IF STATUS = NOTFOUND THEN
            CALL Despliega_com_esq_ispt() 
                 Returning g_reg[arr_c].cod_esq_ispt,
                           g_reg[arr_c].desc_esq_ispt
         END IF
}

      ON KEY ( INTERRUPT )
         CALL Inicializa()
         EXIT INPUT
      ON KEY ( ESC )
         FOR i = 1 TO ARR_CURR()-1
            IF g_reg[i].cod_puesto IS NULL THEN
               ERROR "Puesto NO puede ser NULO"
               NEXT FIELD cod_puesto
            END IF

            SELECT "X" 
            FROM   tab_puesto
            WHERE  cod_puesto = g_reg[i].cod_puesto
            IF STATUS <> NOTFOUND THEN
               ERROR "Puesto YA EXISTE"
               LET g_reg[i].cod_puesto = NULL
               DISPLAY g_reg[i].cod_puesto TO
                       scr_1[i].cod_puesto
               NEXT FIELD cod_puesto
            END IF
            IF g_reg[i].desc_puesto IS NULL THEN
               ERROR "Descripcion Puesto NO puede ser NULO"
               NEXT FIELD desc_puesto
            END IF
            IF g_reg[i].cod_esq_comision IS NULL THEN
               ERROR "Codigo Esquema bono NO puede ser NULO"
               NEXT FIELD cod_esq_comision
            END IF
            IF g_reg[i].cod_esq_bono IS NULL THEN
               ERROR "Codigo Esquema bono NO puede ser NULO"
               NEXT FIELD cod_esq_bono
            END IF
         END FOR

         FOR i = 1 TO ARR_CURR()
            IF g_reg[i].cod_puesto IS NOT NULL THEN
               INSERT INTO tab_puesto VALUES 
                  (g_reg[i].cod_puesto,
                   g_reg[i].desc_puesto,
                   g_reg[i].cod_esq_comision,
                   g_reg[i].cod_esq_bono,
                   0,         ----          g_reg[i].cod_esq_ispt,
                   g_meta.meta_afi1,
                   g_meta.meta_afi2,
                   g_meta.meta_afi3,
                   g_meta.meta_tra1,
 
                   0,
                   0,
                   g_meta.usuario
                  )
            END IF
         END FOR

         ERROR "REGISTRO(S) INGRESADO(S)" SLEEP 2 ERROR ""

         CALL Inicializa()

         EXIT INPUT

      ON KEY (CONTROL-B)
         IF g_reg[arr_c].cod_puesto IS NULL THEN
            ERROR "Captura codigo puesto"
         ELSE
            CALL metas("A",g_reg[arr_c].cod_puesto,g_reg[arr_c].desc_puesto)
         END IF
   END INPUT

END FUNCTION

FUNCTION metas(vcomando,cod_puesto,desc_tipo)
   DEFINE i  		      INTEGER,
          vcomando		CHAR(01),
          cod_puesto	SMALLINT,
          x_puesto	SMALLINT,
          desc_tipo		CHAR(50)

   OPEN WINDOW ventana_meta AT 8,2 WITH FORM "COMM0063" ATTRIBUTE(BORDER)
   DISPLAY "                    Metas de Venta por Tipo Promotor                           " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY hoy USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   IF vcomando="A" THEN
      DISPLAY " [ Esc ] Agrega                [ Ctrl-c ] Salir" AT 1,1
      DISPLAY " AGREGA " AT 1,70 ATTRIBUTE(REVERSE)
   ELSE
--  03/10/2005        DISPLAY " [Esc] Modifica      [Ctrl-v] Historico Metas      [Ctrl-c] Salir" AT 1,1
      DISPLAY " [Esc] Modifica                                                    [Ctrl-c] Salir" AT 1,1
      DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE)
   END IF

   LET x_puesto = cod_puesto

   LET g_meta.meta_afi1  = 0
   LET g_meta.meta_afi2  = 0
   LET g_meta.meta_afi3  = 0
   LET g_meta.meta_tra1  = 0

   DISPLAY BY NAME cod_puesto

   DISPLAY BY NAME desc_tipo

   IF vcomando="A" THEN
      INPUT BY NAME g_meta.meta_afi1,
                    g_meta.meta_afi2,
                    g_meta.meta_afi3,
                    g_meta.meta_tra1

          ON KEY ( ESC )
             EXIT INPUT

          ON KEY ( INTERRUPT )
             LET g_meta.meta_afi1 = 0
             LET g_meta.meta_afi2 = 0
             LET g_meta.meta_afi3 = 0
             LET g_meta.meta_tra1 = 0
             EXIT INPUT

      END INPUT 

   ELSE # vcomando = "M"
    
      SELECT t.meta_afi1,
             t.meta_afi2,
             t.meta_afi3,
             t.meta_tra1 
      INTO   g_meta.meta_afi1,
             g_meta.meta_afi2,
             g_meta.meta_afi3,
             g_meta.meta_tra1 
      FROM   tab_puesto t
      WHERE  t.cod_puesto = x_puesto

      LET g_meta2.meta_afi1 = g_meta.meta_afi1
      LET g_meta2.meta_afi2 = g_meta.meta_afi2
      LET g_meta2.meta_afi3 = g_meta.meta_afi3
      LET g_meta2.meta_tra1 = g_meta.meta_tra1

      INPUT BY NAME g_meta.meta_afi1, 
                    g_meta.meta_afi2,
                    g_meta.meta_afi3,
                    g_meta.meta_tra1 WITHOUT DEFAULTS

          BEFORE FIELD meta_afi1
             LET vhora = TIME

          ON KEY ( ESC )
             
             IF g_meta2.meta_afi1 <> g_meta.meta_afi1  OR
                g_meta2.meta_afi2 <> g_meta.meta_afi2  OR
                g_meta2.meta_afi3 <> g_meta.meta_afi3  OR
                g_meta2.meta_tra1 <> g_meta.meta_tra1  THEN
                

                INSERT INTO tab_his_meta
                   VALUES (TODAY,
                           vhora,
                           x_puesto,
                           g_meta2.meta_afi1,
                           g_meta2.meta_afi2,
                           g_meta2.meta_afi3,
                           g_meta2.meta_tra1,
                           0,
                           0,
                           g_meta.usuario)
             END IF
                           
             UPDATE tab_puesto
             SET    meta_afi1 = g_meta.meta_afi1,
                    meta_afi2 = g_meta.meta_afi2,
                    meta_afi3 = g_meta.meta_afi3,
                    meta_tra1 = g_meta.meta_tra1,
                    usuario   = g_meta.usuario
             WHERE  tab_puesto.cod_puesto = x_puesto
    
             EXIT INPUT

          ON KEY ( INTERRUPT )
             LET g_meta.meta_afi1 = 0
             LET g_meta.meta_afi2 = 0
             LET g_meta.meta_afi3 = 0
             LET g_meta.meta_tra1 = 0
             EXIT INPUT
--  03/10/2005          ON KEY (CONTROL-V)
--  03/10/2005             CALL despliega_hist_metas(x_puesto,desc_tipo)
      END INPUT 

   END IF  #vcomando
    
   CLOSE WINDOW ventana_meta

END FUNCTION

FUNCTION despliega_hist_metas(tipo,desc)
   DEFINE 
      r_pro ARRAY[200] OF RECORD
         vfecha_cambio DATE,
         vhora_cambio  CHAR(08),
         vmeta_afi1    DECIMAL(12,2),
         vmeta_afi2    DECIMAL(12,2),
         vmeta_afi3    DECIMAL(12,2),
         vmeta_tra1    DECIMAL(12,2)
      END RECORD,
      i INTEGER,
      tipo INTEGER,
      desc CHAR(50)


   OPEN WINDOW win1 AT 08,09 WITH FORM "COMM0064" ATTRIBUTE(BORDER)
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " [Ctrl-C] Salir                                                          " AT 2,1 ATTRIBUTE (REVERSE)
   DISPLAY "Codigo Puesto ",tipo CLIPPED," ",desc CLIPPED AT 2,20 ATTRIBUTE(REVERSE)
   DISPLAY " HISTORICO " AT 2,56 ATTRIBUTE(REVERSE)

   DECLARE cur_metas CURSOR FOR
   SELECT fecha_cambio,
          hora_cambio,
          meta_afi1,
          meta_afi2,
          meta_afi3,
          meta_tra1,
          meta_tra2,
          meta_tra3
   FROM   tab_his_meta
   WHERE  cod_puesto = tipo
   ORDER  BY fecha_cambio,hora_cambio
   LET i = 1
   FOREACH cur_metas INTO r_pro[i].*
      LET i = i + 1
   END FOREACH
display "tipo   ",tipo
display "desc   ",desc
display "i   ",i
exit program
   CALL SET_COUNT(i-1)
   DISPLAY ARRAY r_pro to scr_1.* 
      ON KEY (INTERRUPT)
         EXIT DISPLAY
   END DISPLAY
   CLOSE WINDOW win1
END FUNCTION

FUNCTION Trae_datos()

   DECLARE cursor_1 CURSOR FOR 
   SELECT a.cod_puesto,
          a.desc_puesto,
          a.cod_esq_comision,
          b.desc_esq_comision,
          a.cod_esq_bono,
          c.desc_esq_bono
--          a.cod_esq_ispt,
--          d.desc_esq_ispt
   FROM   tab_puesto a,
          com_esq_comis b,
          com_esq_bono c
--          com_esq_ispt d
   WHERE  a.cod_esq_comision = b.cod_esq_comision 
   AND    a.cod_esq_bono     = c.cod_esq_bono
--   AND    a.cod_esq_ispt     = d.cod_esq_ispt
   ORDER  BY 1

END FUNCTION

FUNCTION Consulta()
   DEFINE i,pos SMALLINT

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " CONSULTA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)
   DISPLAY " [Ctrl-b] Metas                                  [Ctrl-c] Salir" AT 1,1 ATTRIBUTE(BOLD)

   CALL Trae_datos()

   LET i = 1

   FOREACH cursor_1 INTO g_reg[i].*
      LET i = i + 1
   END FOREACH

   CALL SET_COUNT(i-1)

   DISPLAY ARRAY g_reg TO scr_1.*
      ON KEY (INTERRUPT)
         CALL Inicializa()
         EXIT DISPLAY
      ON KEY (CONTROL-B)
         LET pos = ARR_CURR()
         CALL Consulta_metas(g_reg[pos].cod_puesto,g_reg[pos].desc_puesto)
   END DISPLAY
END FUNCTION

FUNCTION Consulta_metas(cod_puesto,desc_tipo)
   DEFINE i  		   INTEGER,
          vcomando	CHAR(01),
          cod_puesto	SMALLINT,
          x_puesto	SMALLINT,
          desc_tipo	CHAR(50)

   DEFINE g_met ARRAY[10] OF RECORD 
      meta_afi1 LIKE com_his_meta.meta_afi1,
      meta_afi2 LIKE com_his_meta.meta_afi2,
      meta_afi3 LIKE com_his_meta.meta_afi3,
      meta_tra1 LIKE com_his_meta.meta_tra1
   END RECORD

   OPEN WINDOW ventana_meta AT 8,2 WITH FORM "COMM0065" ATTRIBUTE(BORDER)
   DISPLAY "                    Metas de Venta por Codigo Puesto                           " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY hoy USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
--  03/10/2005          DISPLAY " [Ctrl-c] Salir                     [Ctrl-v] Historico Metas " AT 1,1
      DISPLAY " [Ctrl-c] Salir                                                                " AT 1,1
      DISPLAY " CONSULTA " AT 1,67 ATTRIBUTE(REVERSE)

   DISPLAY BY NAME cod_puesto
   DISPLAY BY NAME desc_tipo

   LET x_puesto = cod_puesto

   SELECT t.meta_afi1,
          t.meta_afi2,
          t.meta_afi3,
          t.meta_tra1 
   INTO   g_met[1].meta_afi1,
          g_met[1].meta_afi2,
          g_met[1].meta_afi3,
          g_met[1].meta_tra1
   FROM   tab_puesto t
   WHERE  t.cod_puesto = x_puesto

   CALL SET_COUNT(1)

   DISPLAY ARRAY g_met to scr_meta.*
      ON KEY ( INTERRUPT )
         LET g_meta.meta_afi1 = 0
         LET g_meta.meta_afi2 = 0
         LET g_meta.meta_afi3 = 0
         LET g_meta.meta_tra1 = 0
         EXIT DISPLAY 
      ON KEY (CONTROL-V)
         CALL despliega_hist_metas(x_puesto,desc_tipo)
   END DISPLAY

   CLOSE WINDOW ventana_meta
END FUNCTION

FUNCTION Modifica()
   DEFINE i SMALLINT
   DEFINE v SMALLINT

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " MODIFICA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)
   DISPLAY " [ Esc ] Modifica         [Ctrl-b] Metas         [ Ctrl-c ] Salir" AT 1,1 ATTRIBUTE(BOLD)

   CALL Trae_datos()

   LET i = 1

   FOREACH cursor_1 INTO g_reg[i].*
      LET i = i + 1
   END FOREACH

   CALL SET_COUNT(i-1)

   INPUT ARRAY g_reg WITHOUT DEFAULTS FROM scr_1.*

      BEFORE FIELD cod_puesto
         LET arr_c = ARR_CURR()
         LET scr_l = SCR_LINE()
         NEXT FIELD desc_puesto

      BEFORE FIELD desc_puesto
         LET arr_c = ARR_CURR()
         LET scr_l = SCR_LINE()

      AFTER FIELD desc_puesto
         IF g_reg[arr_c].desc_puesto IS NULL THEN
            ERROR "Descripcion Puesto NO puede ser NULO"
            NEXT FIELD desc_puesto
         END IF

      BEFORE FIELD cod_esq_comision
         LET arr_c = ARR_CURR()
         LET scr_l = SCR_LINE()

      AFTER FIELD cod_esq_comision
         IF g_reg[arr_c].cod_esq_comision IS NULL THEN
            CALL Despliega_cuadro_comision() 
                 Returning g_reg[arr_c].cod_esq_comision,
                           g_reg[arr_c].desc_esq_comision
         END IF

----        WHILE TRUE
----           IF g_reg[arr_c].cod_esq_comision = 0 THEN
----              CALL Despliega_cuadro_comision() 
----              Returning g_reg[arr_c].cod_esq_comision,
----                        g_reg[arr_c].desc_esq_comision
----           ELSE
----              EXIT WHILE
----           END IF 
----        END WHILE

         SELECT "X" 
         FROM   com_esq_comis
         WHERE  cod_esq_comision = g_reg[arr_c].cod_esq_comision
         IF STATUS = NOTFOUND THEN
            CALL Despliega_cuadro_comision() 
                 Returning g_reg[arr_c].cod_esq_comision,
                           g_reg[arr_c].desc_esq_comision
         ELSE
            SELECT desc_esq_comision 
            INTO   g_reg[arr_c].desc_esq_comision
            FROM   com_esq_comis
            WHERE  cod_esq_comision=g_reg[arr_c].cod_esq_comision

            DISPLAY g_reg[arr_c].desc_esq_comision TO
                    scr_1[scr_l].desc_esq_comision
         END IF

      BEFORE FIELD cod_esq_bono
         LET arr_c = ARR_CURR()
         LET scr_l = SCR_LINE()

      AFTER FIELD cod_esq_bono
         IF g_reg[arr_c].cod_esq_bono IS NULL THEN
            CALL Despliega_cuadro_bono() 
                 Returning g_reg[arr_c].cod_esq_bono,
                           g_reg[arr_c].desc_esq_bono
         ELSE
            SELECT desc_esq_bono 
            INTO   g_reg[arr_c].desc_esq_bono
            FROM   com_esq_bono
            WHERE  cod_esq_bono = g_reg[arr_c].cod_esq_bono

            DISPLAY g_reg[arr_c].desc_esq_bono TO
                    scr_1[scr_l].desc_esq_bono
         END IF

----         WHILE TRUE
----            IF g_reg[arr_c].cod_esq_bono = 0 THEN
----               CALL Despliega_cuadro_bono() 
----               Returning g_reg[arr_c].cod_esq_bono,
----                         g_reg[arr_c].desc_esq_bono
----            ELSE 
----               EXIT WHILE
----            END IF
----         END WHILE

         SELECT "X" 
         FROM    com_esq_bono
         WHERE cod_esq_bono = g_reg[arr_c].cod_esq_bono

         IF STATUS = NOTFOUND THEN
            CALL Despliega_cuadro_bono() 
            Returning g_reg[arr_c].cod_esq_bono,
                      g_reg[arr_c].desc_esq_bono
         END IF

{
      BEFORE FIELD cod_esq_ispt
         LET arr_c = ARR_CURR()
         LET scr_l = SCR_LINE()

      AFTER FIELD cod_esq_ispt
         IF g_reg[arr_c].cod_esq_ispt IS NULL THEN
            CALL Despliega_com_esq_ispt() 
            Returning g_reg[arr_c].cod_esq_ispt,
                      g_reg[arr_c].desc_esq_ispt
         ELSE
            SELECT desc_esq_ispt 
            INTO   g_reg[arr_c].desc_esq_ispt
            FROM   com_esq_ispt
            WHERE  cod_esq_ispt = g_reg[arr_c].cod_esq_ispt

            DISPLAY  g_reg[arr_c].desc_esq_ispt 
                 TO scr_1[scr_l].desc_esq_ispt
         END IF

         WHILE TRUE
            IF g_reg[arr_c].cod_esq_ispt IS NULL THEN
               CALL Despliega_com_esq_ispt() 
                    Returning g_reg[arr_c].cod_esq_ispt,
                              g_reg[arr_c].desc_esq_ispt
            ELSE 
               EXIT WHILE
            END IF
         END WHILE

         SELECT "X" 
         FROM   com_esq_ispt
         WHERE  cod_esq_ispt = g_reg[arr_c].cod_esq_ispt
         IF STATUS = NOTFOUND THEN
            CALL Despliega_com_esq_ispt() 
                 Returning g_reg[arr_c].cod_esq_ispt,
                           g_reg[arr_c].desc_esq_ispt
         END IF
}

      ON KEY ( INTERRUPT )
         CALL Inicializa()
         EXIT INPUT
      ON KEY ( ESC )
         FOR i = 1 TO ARR_CURR()
            IF g_reg[i].cod_puesto IS NOT NULL THEN
               UPDATE tab_puesto 
               SET    desc_puesto      = g_reg[i].desc_puesto,
                      cod_esq_comision = g_reg[i].cod_esq_comision,
                      cod_esq_bono     = g_reg[i].cod_esq_bono
----                      cod_esq_ispt     = g_reg[i].cod_esq_ispt
               WHERE  cod_puesto       = g_reg[i].cod_puesto 
            END IF
         END FOR

         ERROR "REGISTRO(S) MODIFICADO(S)" SLEEP 2 ERROR ""

         CALL Inicializa()

         EXIT INPUT

      ON KEY (CONTROL-B)
         CALL metas("M",g_reg[arr_c].cod_puesto,g_reg[arr_c].desc_puesto)
   END INPUT
#END FOR

END FUNCTION

FUNCTION Elimina()
   DEFINE i SMALLINT

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " ELIMINA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
   DISPLAY "[ Ctrl-c ] Salir" AT 1,1 ATTRIBUTE(BOLD)
   DISPLAY "[ Ctrl-B ] Elimina" AT 1,23 ATTRIBUTE(BOLD)

   CALL Trae_datos()

   LET i = 1

   FOREACH cursor_1 INTO g_reg[i].*
      LET i = i + 1
   END FOREACH

   CALL SET_COUNT(i-1)

   DISPLAY ARRAY g_reg TO scr_1.*
      ON KEY ( CONTROL-B )
         LET i = ARR_CURR()
         IF Esta_seguro() THEN
            DELETE 
            FROM   tab_puesto
            WHERE  cod_puesto = g_reg[i].cod_puesto
            ERROR "REGISTRO ELIMINADO" SLEEP 2 ERROR ""
         ELSE
            ERROR "ELIMINAR CANCELADO" SLEEP 2 ERROR ""
         END IF

         CALL Inicializa()

         EXIT DISPLAY

      ON KEY ( INTERRUPT )
         CALL Inicializa()
         EXIT DISPLAY
   END DISPLAY

END FUNCTION

FUNCTION Esta_seguro()

   PROMPT "Desea Eliminar Registro S/N " FOR CHAR aux_pausa
   IF aux_pausa MATCHES "[sS]" THEN
      RETURN TRUE
   END IF

   RETURN FALSE
END FUNCTION

FUNCTION Despliega_cuadro_comision()
   DEFINE aux_val SMALLINT

   DEFINE l_reg ARRAY[1000] OF RECORD
      codigo      INTEGER,
      descripcion CHAR(50)
   END RECORD

   DEFINE x_x      char(200),
          x_buscar char(30)

   DEFINE pos SMALLINT

   OPEN WINDOW vent_1 AT 05,12 WITH FORM "COMM0062" ATTRIBUTE(BORDER)
   DISPLAY "                   C O M I S I O N E S                   " AT 2,1 ATTRIBUTE(REVERSE)

   INPUT BY NAME x_buscar
      AFTER FIELD x_buscar
         IF x_buscar IS NULL THEN
            ERROR "Descripcion a Buscar NO puede ser nulo"
            NEXT FIELD x_buscar
         ELSE
            EXIT INPUT
         END IF
   END INPUT                             

   WHILE TRUE
      LET x_x = "SELECT cod_esq_comision,desc_esq_comision ",
                "FROM   com_esq_comis ",
               " WHERE  desc_esq_comision MATCHES ",'"',x_buscar CLIPPED,'"',
               " ORDER  BY 1 " CLIPPED

      PREPARE curcua1 FROM x_x
      DECLARE curcua CURSOR FOR curcua1

      LET pos = 1

      FOREACH curcua INTO l_reg[pos].*
         LET pos = pos + 1

         IF pos >= 1000 THEN
            ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
            EXIT FOREACH
         END IF
      END FOREACH

      IF (pos-1) < 1 THEN
         ERROR "ARCHIVO COMISIONES... VACIO"
      END IF

      CALL SET_COUNT(pos-1)

      DISPLAY ARRAY l_reg TO scr_1.*
         ON KEY ( INTERRUPT )
            LET pos = 0
            EXIT DISPLAY
         ON KEY ( CONTROL-M )
            LET pos = ARR_CURR()
            EXIT DISPLAY
      END DISPLAY

      IF pos <> 0 THEN
         EXIT WHILE
      END IF

   END WHILE

   CLOSE WINDOW vent_1

   RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION

FUNCTION Despliega_cuadro_bono()
   DEFINE aux_val          SMALLINT

   DEFINE l_reg ARRAY[1000] OF RECORD
      codigo      INTEGER,
      descripcion CHAR(50)
   END RECORD

   DEFINE x_x      char(200),
          x_buscar char(30)

   DEFINE pos SMALLINT

   OPEN WINDOW vent_1 AT 05,12 WITH FORM "COMM0062" ATTRIBUTE(BORDER)
   DISPLAY "                       B  O  N  O  S                     " AT 2,1 ATTRIBUTE(REVERSE)

   INPUT BY NAME x_buscar
      AFTER FIELD x_buscar
         IF x_buscar IS NULL THEN
            ERROR "Descripcion a Buscar NO puede ser nulo"
            NEXT FIELD x_buscar
         ELSE
            EXIT INPUT
         END IF
   END INPUT

   WHILE TRUE
      LET x_x = " SELECT cod_esq_bono,desc_esq_bono FROM com_esq_bono ",
                " WHERE desc_esq_bono MATCHES ",'"',x_buscar CLIPPED,'"',
                " ORDER BY 1 " CLIPPED

      PREPARE curcu1 FROM x_x
      DECLARE curcu CURSOR FOR curcu1

      LET pos = 1

      FOREACH curcu INTO l_reg[pos].*
         LET pos = pos + 1
         IF pos >= 1000 THEN
            ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
            EXIT FOREACH
         END IF
      END FOREACH

      IF (pos-1) < 1 THEN
         ERROR "ARCHIVO COMISIONES... VACIO"
      END IF

      CALL SET_COUNT(pos-1)

      DISPLAY ARRAY l_reg TO scr_1.*
         ON KEY (INTERRUPT)
            LET pos = 0
            EXIT DISPLAY
         ON KEY ( CONTROL-M )
            LET pos = ARR_CURR()
            EXIT DISPLAY
      END DISPLAY

      IF pos <> 0 THEN
         EXIT WHILE
      END IF

   END WHILE

   CLOSE WINDOW vent_1

   RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION

FUNCTION Despliega_com_esq_ispt()

   DEFINE aux_val SMALLINT
   DEFINE l_reg ARRAY[1000] OF RECORD
      codigo           INTEGER,
      descripcion      CHAR(50)
   END RECORD

   DEFINE x_x      char(200),
          x_buscar char(30)

   DEFINE pos SMALLINT

   OPEN WINDOW vent_1 AT 05,12 WITH FORM "COMM0062" ATTRIBUTE(BORDER)
   DISPLAY "             T I P O   D E   I M P U E S T O S           " AT 2,1 ATTRIBUTE(REVERSE)

   INPUT BY NAME x_buscar
      AFTER FIELD x_buscar
         IF x_buscar IS NULL THEN
            ERROR "Descripcion a Buscar NO puede ser nulo"
            NEXT FIELD x_buscar
         ELSE
            EXIT INPUT
         END IF
   END INPUT                             

   WHILE TRUE
      LET x_x = " SELECT cod_esq_ispt,desc_esq_ispt FROM com_esq_ispt ",
                " WHERE desc_esq_ispt MATCHES ",'"',x_buscar CLIPPED,'"',
                " ORDER BY 1 " CLIPPED

      PREPARE curcu110 FROM x_x
      DECLARE curcu10 CURSOR FOR curcu110

      LET pos = 1

      FOREACH curcu10 INTO l_reg[pos].*
         LET pos = pos + 1
         IF pos >= 1000 THEN
            ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
            EXIT FOREACH
         END IF
      END FOREACH

      IF (pos-1) < 1 THEN
         ERROR "ARCHIVO COMISIONES... VACIO"
      END IF

      CALL SET_COUNT(pos-1)

      DISPLAY ARRAY l_reg TO scr_1.*
         ON KEY ( INTERRUPT )
            LET pos = 0
            EXIT DISPLAY
         ON KEY ( CONTROL-M )
            LET pos = ARR_CURR()
            EXIT DISPLAY
      END DISPLAY

      IF pos <> 0 THEN
         EXIT WHILE
      END IF

   END WHILE

   CLOSE WINDOW vent_1
   RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION

FUNCTION Actualiza_metas()
   DEFINE reg4 RECORD
      metas    INTEGER,
      agencia  CHAR(10)
   END RECORD

   DEFINE reg3 RECORD
      metas    INTEGER,
      agencia  CHAR(10)
   END RECORD

   DEFINE reg2 RECORD
      metas    INTEGER,
      agencia  CHAR(10)
   END RECORD

   DEFINE reg1 RECORD
      metas    INTEGER,
      agencia  CHAR(10)
   END RECORD

   DEFINE vcod_resp_uni CHAR(10),
          vpuesto_cod   SMALLINT

   ERROR "Actualizando Metas"
 
   ---- ACTUALIZA METAS NIVEL 4  ----
   DECLARE cur_4 CURSOR FOR
   SELECT count(*),agenc_cod[1,2]
   FROM   pro_mae_promotor,com_nivel1
   WHERE  agenc_cod=coduni_n1
   AND    nombre_uni_n1 not matches 'HIST*'
   GROUP BY 2
   ORDER BY 2

   FOREACH cur_4 INTO reg4.*
      SELECT cod_resp_uni 
      INTO   vcod_resp_uni
      FROM   com_dat_uni_com
      WHERE  nivel = 4
      AND    cod_uni = reg4.agencia

      SELECT puesto_resp
      INTO   vpuesto_cod
      FROM   com_respon_unidad
      WHERE  cod_resp_uni = vcod_resp_uni

      UPDATE tab_puesto
      SET    metas = reg4.metas
      WHERE  cod_puesto = vpuesto_cod

   END FOREACH

   ---- ACTUALIZA METAS NIVEL 3  ----

   DECLARE cur_3 CURSOR FOR
   SELECT count(*),agenc_cod[1,4]
   FROM   pro_mae_promotor,com_nivel1
   WHERE  agenc_cod=coduni_n1
   AND    nombre_uni_n1 not matches 'HIST*'
   GROUP  BY 2
   ORDER  BY 2

   FOREACH cur_3 INTO reg3.*
      SELECT cod_resp_uni 
      INTO   vcod_resp_uni
      FROM   com_dat_uni_com
      WHERE  nivel = 3
      AND    cod_uni = reg3.agencia

      SELECT puesto_resp
      INTO   vpuesto_cod
      FROM   com_respon_unidad
      WHERE  cod_resp_uni = vcod_resp_uni

      UPDATE tab_puesto
      SET    metas = reg3.metas
      WHERE  cod_puesto = vpuesto_cod

   END FOREACH

   ERROR""

END FUNCTION
