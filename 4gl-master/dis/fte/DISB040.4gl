######################################################################
#Proyecto            => Sistema de Afores. (MEXICO)                  #
#Propietario         => E.F.P.                                       #
#Sistema             => DIS                                          #
#Programa            => DISB040                                      #
#Descripcion         => Reverso de Provision y Liquidacion Int Trans.#
#Fecha               => 16 de JULIO de 2002                          #
#Por                 => GERARDO ALFONSO VEGA PAREDES.                #
#Modificado          => 19 Abril 2006 JOSE ALEJANDRO RAMIREZ         #
#Descripcion         => (v4) Se agrega el borrado a dis_ctrl_proceso #
#--------------------------------------------------------------------#
#Modificado          => 23 junio 2006 Jose Alejandro Ramirez         #
#Descripcion         => c22-11 Cambios de circular                   #
#--------------------------------------------------------------------#
#Modificado          => 27 abril 2009 DMR,Gonzalo Hernandez          #
#Descripcion         => borrado de tabla tmp_pla_rcv y arch. generado#
######################################################################
DATABASE safre_af

GLOBALS
   DEFINE hoy       DATE,
	  opc       CHAR(01)

   DEFINE vfolio    INTEGER,
	  vpasswd   CHAR(01),
	  vnip      INTEGER

   DEFINE g_reg4 RECORD
      super_cod     SMALLINT,
      super_desc    CHAR(30),
      nip           INTEGER
   END RECORD

   DEFINE vfec      DATE    --v4
END GLOBALS


MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-O

   DEFER INTERRUPT

   LET vpasswd = "N"

   CALL Aplica_passwd() RETURNING vpasswd

   IF vpasswd = "S" THEN
      ERROR "ACCESO ACEPTADO"
      SLEEP 2
      ERROR ""
      CALL inicio()
   END IF
END MAIN


FUNCTION Aplica_passwd()
   OPEN WINDOW ventana_4 AT 08,12 WITH FORM "DISB0403" ATTRIBUTE(BORDER)
   DISPLAY " [Esc] Procesar      [DISB040]      [Ctrl-c] Cancelar " AT 2,1 ATTRIBUTE(REVERSE)

   INPUT BY NAME g_reg4.*
      AFTER FIELD super_cod
	 IF g_reg4.super_cod IS NULL THEN
	    ERROR "ID NO PUEDE SER NULO"
	    NEXT FIELD super_cod
         END IF

         SELECT super_desc,
		nip
         INTO   g_reg4.super_desc,
		vnip
         FROM   tab_supervisor
	 WHERE  super_cod = g_reg4.super_cod

	 IF STATUS = NOTFOUND THEN
	    ERROR "NO EXISTE ESTE ID"
	    NEXT FIELD super_cod
         END IF

	 DISPLAY BY NAME g_reg4.super_desc
	 NEXT FIELD nip

      AFTER FIELD nip
         IF g_reg4.nip <> vnip THEN
            ERROR "CLAVE DE ACCESO NO EXISTE"
            LET g_reg4.nip = null
            NEXT FIELD super_cod
         END IF

         IF g_reg4.nip IS NULL OR g_reg4.nip = 0 THEN
            ERROR "CLAVE DE ACCESO NO EXISTE"
            NEXT FIELD super_cod
         END IF

      ON KEY (ESC)
         IF g_reg4.super_cod IS NULL THEN
            ERROR "ID NO PUEDE SER NULO"
            NEXT FIELD super_cod
         END IF

         SELECT "x"
           FROM tab_supervisor 
          WHERE super_cod = g_reg4.super_cod

         IF STATUS = NOTFOUND THEN
            ERROR "NO EXISTE ESTE ID "
            NEXT FIELD super_cod
         END IF

         IF g_reg4.nip IS NULL OR
            g_reg4.nip = 0 THEN
            ERROR "CLAVE DE ACCESO NO EXISTE"
            NEXT FIELD super_cod
         END IF

         IF g_reg4.nip <> vnip THEN
            ERROR "CLAVE DE ACCESO NO EXISTE"
            LET g_reg4.nip = null
            NEXT FIELD super_cod
         END IF

         LET vpasswd = "S"
         EXIT INPUT

      ON KEY (INTERRUPT)
         ERROR "ACCESO DENEGADO"
         SLEEP 2
         LET vpasswd = "N"
         EXIT INPUT
          
   END INPUT

   CLOSE WINDOW ventana_4
   RETURN vpasswd
END FUNCTION


FUNCTION inicio()
  LET hoy = TODAY

  OPEN WINDOW ventana AT 3,4 WITH 3 ROWS, 72 COLUMNS ATTRIBUTE(BORDER)

  DISPLAY " DISB040 " AT 3,1 ATTRIBUTE(REVERSE)
  DISPLAY hoy USING " dd-mm-yyyy " AT 3,60 ATTRIBUTE(REVERSE)

   MENU "Reverso Int Transito RCV" 
      COMMAND "Reverso 1" "Provision Intereses Transito RCV."
         CALL Reverso_1()
      COMMAND "Reverso 2" "Liquidacion Intereses Transito RCV."
         CALL Reverso_2()
      COMMAND "Salir" "Salir del menu principal."
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana
END FUNCTION


FUNCTION Reverso_1()
   DEFINE w_param2             LIKE dis_ctrl_proceso.parametro1
   DEFINE d_fecha_creac_lote   DATE 
   DEFINE carga_reg            CHAR(360)

   CLEAR SCREEN

   OPEN WINDOW ventana_1 AT 8,4 WITH FORM "DISB0401" ATTRIBUTE(BORDER)

   DISPLAY " (Control-c) Salir                               (ESC) Ejecutar                " AT 1,1 ATTRIBUTE(REVERSE,green)

   LET INT_FLAG = FALSE

   INPUT BY NAME vfolio
      ON KEY (ESC)
	 LET INT_FLAG = FALSE
	 EXIT INPUT
      ON KEY (control-c)
	 LET INT_FLAG = TRUE
	 EXIT INPUT
   END INPUT

   IF INT_FLAG = TRUE THEN
      LET INT_FLAG = FALSE
      ERROR "PROCESO CANCELADO..."
      SLEEP 2
      ERROR ""
      CLEAR FORM
      CLEAR SCREEN
      CLOSE WINDOW ventana_1
      RETURN
   END IF

   --- ### 27 Abr 2009 Se incluye validación no exista estatales
   SELECT UNIQUE folio
   FROM   cta_interes_rcv
   WHERE  folio = vfolio
   AND    subcuenta in (5, 6, 9)          
   AND    tipo_movimiento = 3 

   IF SQLCA.SQLCODE = NOTFOUND THEN
       --- ### Display " Ok no hay estatales .. '    
   ELSE 
       ERROR "PRIMERO DEBE REVERSAR LAS SUBCUENTAS ESTATALES DE ESTE FOLIO "
       SLEEP 5
       ERROR ""
       CLEAR FORM
       CLEAR SCREEN
       CLOSE WINDOW ventana_1
       RETURN
   END IF
   --- ###

   SELECT UNIQUE folio
   FROM   cta_interes_rcv
   WHERE  folio = vfolio
   AND    subcuenta in (1,2,3,11,15,17)  --c22-11

   IF SQLCA.SQLCODE = NOTFOUND THEN
      ERROR "FOLIO NO ENCONTRADO..."
      SLEEP 2
      ERROR ""
      CLEAR WINDOW ventana_1
      CLOSE WINDOW ventana_1
      RETURN
   ELSE
      PROMPT "Deseas hacer el reverso [S/N]..." FOR opc
      IF opc MATCHES "[Ss]" THEN
	 ERROR "PROCESANDO INFORMACION..."

	 DELETE 
	 FROM   cta_interes_rcv
	 WHERE  folio = vfolio
         AND    subcuenta in (1,2,3,11,15,17)               --c22-11
	 AND    tipo_movimiento = 3

         SELECT max(fecha_proceso)                          --v4
         INTO   vfec                                        --v4
         FROM   dis_ctrl_proceso                            --v4
         WHERE  proceso_cod = 'DISB011'                     --v4
         AND    folio = vfolio                              --v4

         ### APYRON  22 Abr 2009 
         ### Cambio instruccion DELETE

         ### DELETE FROM dis_ctrl_proceso                   --v4
         ### WHERE  proceso_cod='DISB011'                   --v4
         ### AND    fecha_proceso=vfec                      --v4
         ### AND    parametro2 LIKE '%LOTE%'                --v4

         ### DELETE                                
         ### FROM   dis_ctrl_proceso              
         ### WHERE  proceso_cod = 'DISB011'      
         ### AND    folio = vfolio              

         SELECT UNIQUE parametro2 
           INTO w_param2
           FROM dis_ctrl_proceso 
          WHERE proceso_cod   = 'DISB011' 
            AND fecha_proceso = vfec 
            AND etapa_cod     = 1

         DELETE 
           FROM dis_ctrl_proceso
          WHERE fecha_proceso = vfec 
            AND proceso_cod IN ('DISB011','DISB026')
     
         CALL f_borra_archivo(w_param2)    
         
         DECLARE c_sel_tmp CURSOR FOR 
         SELECT *, rowid 
           FROM safre_tmp:tmp_pla_rcv
         ORDER BY rowid 
      
         FOREACH c_sel_tmp   INTO carga_reg
            LET d_fecha_creac_lote  = mdy(carga_reg[21,22],
                                          carga_reg[23,24],
                                          carga_reg[17,20])
            EXIT FOREACH
         END FOREACH

         DELETE FROM cta_cza_transito
          WHERE fecha_creac_lote = d_fecha_creac_lote 

         DELETE FROM cta_det_transito 
          WHERE fecha_creac_lote = d_fecha_creac_lote 
         
         DELETE FROM cta_sum_transito 
          WHERE fecha_creac_lote = d_fecha_creac_lote 
       
         DATABASE safre_tmp 
         DROP TABLE tmp_pla_rcv
         DATABASE safre_af 

         ERROR ""

	 PROMPT "Proceso finalizado oprima [Enter] para salir..." for opc
      ELSE
	 ERROR "PROCESO CANCELADO..."
	 SLEEP 2
      END IF
      CLEAR SCREEN
      CLOSE WINDOW ventana_1
   END IF
END FUNCTION


FUNCTION Reverso_2()
   DEFINE    fech_proces     DATE
   DEFINE    fech_conv       DATE

   CLEAR SCREEN

   OPEN WINDOW ventana_1 AT 8,4 WITH FORM "DISB0401" ATTRIBUTE(BORDER)
   DISPLAY " (Control-c) Salir                               (ESC) Ejecutar                " AT 1,1 ATTRIBUTE(REVERSE,green)

   LET INT_FLAG = FALSE

   INPUT BY NAME vfolio
      ON KEY (ESC)
	 LET INT_FLAG = FALSE
	 EXIT INPUT
      ON KEY (control-c)
	 LET INT_FLAG = TRUE
	 EXIT INPUT
   END INPUT

   IF INT_FLAG = TRUE THEN
      LET INT_FLAG = FALSE
      ERROR "PROCESO CANCELADO..."
      SLEEP 2
      ERROR ""
      CLEAR FORM
      CLEAR SCREEN
      CLOSE WINDOW ventana_1
      RETURN
   END IF

   --- ### 27 Abr 2009 Se incluye validación no exista estatales
   SELECT UNIQUE folio
   FROM   dis_cuenta
   WHERE  folio = vfolio
   AND    subcuenta in (5, 6, 9)          
   AND    tipo_movimiento = 3 

   IF SQLCA.SQLCODE = NOTFOUND THEN
       --- ### Display " Ok no hay estatales .. '    
   ELSE 
       ERROR "PRIMERO DEBE REVERSAR LAS SUBCUENTAS ESTATALES DE ESTE FOLIO "
       SLEEP 5
       ERROR ""
       CLEAR FORM
       CLEAR SCREEN
       CLOSE WINDOW ventana_1
       RETURN
   END IF
   --- ###

   SELECT UNIQUE folio
   FROM   dis_cuenta
   WHERE  folio = vfolio
   AND    subcuenta in (1,2,3,11,15,17)  --c22-11
   AND    tipo_movimiento = 3
   AND    estado = 7

   IF SQLCA.SQLCODE = NOTFOUND THEN
      ERROR "FOLIO NO ENCONTRADO..."
      SLEEP 2
      ERROR ""
      CLEAR WINDOW ventana_1
      CLOSE WINDOW ventana_1
      RETURN
   ELSE
      PROMPT "Deseas hacer el reverso [S/N]..." FOR opc
      IF opc MATCHES "[Ss]" THEN
	 ERROR "PROCESANDO INFORMACION..."

         SELECT UNIQUE fecha_conversion
         INTO fech_conv
         FROM dis_cuenta
         WHERE folio = vfolio
         AND   subcuenta = 3
         AND   tipo_movimiento = 3

	 DELETE 
	 FROM   dis_cuenta
	 WHERE  folio = vfolio
         AND    subcuenta in (1,2,3,11,15,17)  --c22-11
	 AND    tipo_movimiento = 3
	 AND    estado = 7

         DELETE 
         FROM   cta_saldo_vol
         WHERE  folio = vfolio
         AND    fecha_conversion = fech_conv
         AND    consecutivo_lote = 0 

         SELECT UNIQUE fecha_proceso 
           INTO fech_proces 
           FROM dis_ctrl_proceso
          WHERE folio = vfolio
            AND proceso_cod = 'DISB012'
 
         DELETE 
           FROM dis_ctrl_proceso
          WHERE fecha_proceso = fech_proces
            AND proceso_cod IN ('DISB012','DISB078B')

         ERROR ""
	 PROMPT "Proceso finalizado oprima [Enter] para salir..." for opc
      ELSE
	 ERROR "PROCESO CANCELADO..."
	 SLEEP 2
      END IF
      CLEAR SCREEN
      CLOSE WINDOW ventana_1
   END IF
END FUNCTION


FUNCTION f_borra_archivo(w_param2)
   DEFINE w_param2      LIKE dis_ctrl_proceso.parametro1
   DEFINE w_direccion   CHAR(100)
   DEFINE w_year        CHAR(4)
   DEFINE d_year        DECIMAL(4,0) 
   DEFINE d_ini         INTEGER
   DEFINE d_fin         INTEGER
   DEFINE w_final       CHAR(50)
   DEFINE i             SMALLINT
   DEFINE l_paso        CHAR(20)

   SELECT ruta_envio
     INTO w_direccion
     FROM seg_modulo
    WHERE modulo_cod = 'dis'

   IF  SQLCA.SQLCODE = NOTFOUND THEN  
       ERROR "No hay dirección para buscar archivo de envío "
       SLEEP 2
   END IF 
   LET d_year      = YEAR(TODAY)
   LET w_year      = d_year
   LET w_final     = '.TRAN' 
   LET d_ini       = 0
   LET d_fin       = 0

   FOR i = 1 TO LENGTH(w_param2)
       ###IF w_param2[i, i] = 'F' THEN 
       ###   LET   d_ini  =  i 
       ###END IF
       IF w_param2[i,i] = '.' THEN 
          LET   d_fin  =  i
          LET d_ini = d_fin - 5
          IF  d_ini < 1 THEN
              LET d_ini = 1
          END IF
          EXIT FOR
       END IF
   END FOR

   LET d_ini = d_ini - 1
   LET d_fin = d_fin - 1 
   IF  d_ini < 1 OR d_fin < 1 THEN
      ERROR "No hay nombre de archivo para borrar "
      SLEEP 5
   ELSE       
      LET l_paso = w_year[1,2]
      ### Display " el anioo .. ", l_paso
      ### sleep 5
      
      ### Display " el nombri .. ", w_param2, " .i. ", d_ini, " .2. ", d_fin
      ### SLEEP 5
      LET l_paso = w_param2[d_ini, d_fin]
      ### Display " el nombri .. ", l_paso, " .i. ", d_ini, " .2. ", d_fin
      ### sleep 5
   
      LET w_direccion = 'rm '||w_direccion CLIPPED||'/'||w_year[1,2] CLIPPED||w_param2[d_ini, d_fin] CLIPPED||w_final CLIPPED 
      RUN w_direccion 
   END IF
END FUNCTION 

