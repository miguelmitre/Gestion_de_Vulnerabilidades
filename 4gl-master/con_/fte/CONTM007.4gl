########################################################################### 
#Proyecto          => Sistema de Aforer.( MEXICO )                        # 
#Owner             => E.F.P. 					          #
#Programa CONTM007 => GENERACION DE MOVIMIENTOS CONTABLES PROMOTORES      #
#Fecha             => Mayo 27 1998                                        #
#By                => JOSE MANUEL VIZCAINO                                #
#Sistema           => CONTABILIDAD 		                          #
###########################################################################

DATABASE safre_af
 GLOBALS
     DEFINE vreferencia       DECIMAL(10,0)
     DEFINE vtotal            CHAR(17)
     DEFINE vvtotal           CHAR(16)
     DEFINE HOY               DATE
     DEFINE vresp             CHAR(1)
     DEFINE vcodigo           CHAR(10)
END GLOBALS

     MAIN
         
         OPTIONS INPUT  WRAP,
         PROMPT LINE LAST,
         ACCEPT KEY CONTROL-I,
         FORM LINE 3
         DEFER INTERRUPT
 
 	 LET HOY = TODAY
	 
         OPEN WINDOW ventana_1 AT 3,3 WITH FORM "CONTM071" ATTRIBUTE (BORDER)
	 DISPLAY " CONTM007    GENERADOR ARCHIVO tmp_pla CONTABILIDAD PROMOTORES                               " AT 3,1 
         ATTRIBUTE(REVERSE) 
         --DISPLAY HOY USING "dd-mm-yyyy" AT 3,60 
         --ATTRIBUTE(REVERSE)
         
         PROMPT "Desea Generar el Proceso ? S/N "
         ATTRIBUTE (REVERSE)
         FOR vresp
         ATTRIBUTE (REVERSE)

         IF vresp MATCHES "[sS]" THEN
            CALL principal()
            CALL actualiza_contab_pro_plano()
            CALL reporte()
           ELSE
            ERROR "PROCESO CANCELADO ..."
            ATTRIBUTE (REVERSE)
            SLEEP 2
            ERROR " "
         END IF
     
   END MAIN
 ######################################################################### 
   FUNCTION PRINCIPAL() 
 #########################################################################


          DEFINE vnombre                   CHAR(6)
	  DEFINE G_LISTA                   CHAR(200)         
	  DEFINE vreporte                  INTEGER
	  DEFINE vreferencia               CHAR(10)  
	  DEFINE vpregunta                 CHAR(1)
	  DEFINE venvio                    CHAR(2)         
	  DEFINE vtran                     CHAR(2)
	  DEFINE vusuario                  CHAR(10)
	  DEFINE vfecha_proceso            DATE
          DEFINE vfecha_calculo            CHAR(7)
          DEFINE vhoy                      DATE
          DEFINE vanio                     CHAR(4)
          DEFINE vfol_rec                  INTEGER   
          DEFINE vtipo_registro            CHAR(1)
          DEFINE vconstante                CHAR(5)
          DEFINE vfecha_pago               DATE       
          DEFINE vfecha                    CHAR(20)
          DEFINE vfolio_interno            CHAR(10)
          DEFINE vtransa_cod               CHAR(5)
          DEFINE vtransa_desc              CHAR(35)
          DEFINE vdebito_credito           CHAR(1)
          DEFINE vprovi_acred              CHAR(1)
          DEFINE vperiodo                  CHAR(7)
          DEFINE vfecha_hasta              DATE
 
          DEFINE vcodven                   DECIMAL(10,0)
          DEFINE vvcodven                  DECIMAL(10,0)
          DEFINE vtotal_ingreso            DECIMAL(16,6)
          DEFINE vsuma                     DECIMAL(16,6)
          DEFINE vmonto_iva                DECIMAL(16,6)
          DEFINE vmonto_impto              DECIMAL(16,6)
          DEFINE vcont                     SMALLINT
          DEFINE i                         SMALLINT
          DEFINE vtipo_impuesto            CHAR(1)
          DEFINE vcod_tabla_ispt           SMALLINT
          DEFINE vvcod_tabla_ispt          SMALLINT
          DEFINE vmonto                    DECIMAL(16,2)
          DEFINE vflag                     SMALLINT
          DEFINE vbruto                    DECIMAL(16,6)

          LET vhoy  = today
          LET vanio = year(vhoy)
          LET vtipo_registro = "M"
          LET vconstante = "TEST"
          LET vfecha_calculo = today
          LET vfecha_pago = today
          LET vfolio_interno = "REFERENCIA"
          LET vfecha = TODAY,  TIME
 
          WHENEVER ERROR CONTINUE
          DROP TABLE contab_pro_plano
          CREATE TABLE contab_pro_plano
          (
                 codigo_cuenta      CHAR(15),
                 periodo_contab     CHAR(7),
                 fecha_transacc     DATE,
                 tipo_registro      CHAR(1),
                 monto              DECIMAL(16,6),
                 debito_credito     CHAR(1),
                 tipo_diario        CHAR(5),
                 referencia         CHAR(21),
                 desc_conc_mov      CHAR(25),
                 codigo_analisis1   CHAR(15),
                 codigo_analisis2   CHAR(15),
                 codigo_analisis3   CHAR(15),
                 codigo_analisis4   CHAR(15)
           );
          
          WHENEVER ERROR STOP
 
          DECLARE cur cursor FOR 
          SELECT codven, total_ingreso,
          total_premio + total_comision_pro + total_otras_percep + total_bono,
             monto_iva, monto_impto, fecha_hasta, cod_tabla_impto, codven
              FROM com_ingres_resum
          
          LET vcodven = 0
          LET vtransa_cod = 0
          LET vflag = 0
          
          FOREACH cur INTO vcodven,  vtotal_ingreso, vbruto, vmonto_iva, 
                           vmonto_impto, vfecha_hasta, vvcod_tabla_ispt,
                           vcodigo
 
          LET i = 0
          FOR i = 1 TO 4       
             LET vcont = vcont + 1   
             
             IF i = 1 THEN
                 LET vtransa_cod = "6100"  -- importe neto a pagar
                 LET vmonto = vtotal_ingreso
             END IF
             IF i = 2 THEN 
                 LET vtransa_cod = "6102"  -- total comisiones a pagar
                 LET vcodven = "6102"  
                 LET vmonto = vbruto
             END IF
             
             IF i = 3 THEN 
                SELECT cod_tabla_ispt INTO vcod_tabla_ispt FROM tabla_ispt
                     WHERE cod_tabla_ispt = vvcod_tabla_ispt
                IF vcod_tabla_ispt = 1 THEN 
                    LET vcodven ="6103" 
                    LET vmonto = vmonto_impto
                   ELSE 
                    LET vcodven ="6104"
                    LET vmonto = vmonto_impto
                END IF
             END IF 

             IF i = 4 THEN 
                SELECT cod_tabla_ispt INTO vcod_tabla_ispt FROM tabla_ispt
                     WHERE cod_tabla_ispt = vvcod_tabla_ispt
                IF vcod_tabla_ispt = 1 THEN 
                    LET vcodven = "6105"
                    LET vmonto = vmonto_iva
                 ELSE
                    LET vcodven = "666"  
                END IF
             END IF

 
          DISPLAY "CODIGO PROMOTOR :", vcodven AT 10,22
          DISPLAY "GRAN TOTAL      :", vtotal_ingreso AT 11,22
          DISPLAY "SUMA            :", vsuma  AT 12,22
          DISPLAY "MONTO IVA       :", vmonto_iva AT 13,22
          DISPLAY "MONTO ISR       :", vmonto_impto AT 14,22
          DISPLAY "PROCESANDO      :", vcont AT 15,22
          DISPLAY "CONTADOR        :", i AT 16,22
 
          -- actualiza contab_pro_plano

          SELECT transa_cod, transa_desc, debito_credito, provi_acred
              INTO vtransa_cod, vtransa_desc, vdebito_credito, vprovi_acred  
              FROM tab_transaccion
          WHERE transa_cod = vtransa_cod

         CALL Calc_dias(vhoy) RETURNING vfecha_calculo
         LET vperiodo = vanio, vfecha_calculo USING "&&&" 
         LET vperiodo = vperiodo USING "&&&&&&&"
         LET vtransa_desc= "PROVISION POR SUBCUENTA"
	
 
	 INSERT INTO contab_pro_plano 
            VALUES (vcodven,                       -- Codigo Cuenta 
                    vperiodo,                      -- Periodo Contable
                    vfecha_hasta,                  -- Fecha Transaccion
                    vtipo_registro,                -- Tipo Registro
                    vmonto,                        -- Monto
                    vdebito_credito,               -- Debito/Credito
                    vconstante,                    -- Tipo Diario
                    vcodigo,                       -- Referencia
                    vtransa_desc,                  -- Descripcion Conc Mov      
                    " ",                           -- Codigo Analisis 1    
                    " ",                           -- Codigo Analisis 2
                    " ",                           -- Codigo Analisis 3
                    "    " )                       -- Codigo Analisis 4

 
       END FOR
    END FOREACH
 END FUNCTION


#############################################################################
        FUNCTION calc_dias(vdias)
#############################################################################
        
        DEFINE vdias     DATE
        DEFINE vvdias    SMALLINT
        DEFINE vacum     SMALLINT
        DEFINE vmonth    SMALLINT

        LET vvdias = day(vdias)
        LET vmonth  = month(vdias)

        CASE
             WHEN vmonth = 1 
               LET vacum = vvdias
             WHEN vmonth = 2 
               LET vacum = 31 + vvdias
             WHEN vmonth = 3 
               LET vacum = 59 + vvdias
             WHEN vmonth = 4 
               LET vacum = 89 + vvdias
             WHEN vmonth = 5 
               LET vacum = 120 + vvdias
             WHEN vmonth = 6 
               LET vacum = 150 + vvdias
             WHEN vmonth = 7 
               LET vacum = 181 + vvdias
             WHEN vmonth = 8 
               LET vacum = 212 + vvdias
             WHEN vmonth = 9 
               LET vacum = 242 + vvdias
             WHEN vmonth = 10 
               LET vacum = 271 + vvdias
             WHEN vmonth = 11 
               LET vacum = 303 + vvdias
             WHEN vmonth = 12 
               LET vacum = 334 + vvdias
          END CASE
      
     RETURN vacum 
END FUNCTION

 #########################################################################
   FUNCTION reporte()
 #########################################################################
  
   DEFINE gr_report RECORD
          codigo_cuenta            CHAR(10),
          periodo_contab           CHAR(7),
          fecha_transacc           DATE,
          tipo_registro            CHAR(1),
          monto                    DECIMAL(16,2),
          debito_credito           char(1),
          tipo_diario              char(5),
          referencia               char(21),
          desc_conc_mov            char(25),
          codigo_analisis1         char(15),
          codigo_analisis2         char(15),
          codigo_analisis3         char(15),
          codigo_analisis4         char(15)
    END RECORD

    DEFINE vnombre    CHAR(10)
    DEFINE G_LISTA    CHAR(30)
    DEFINE HOY        DATE


    LET HOY = DATE
    LET vnombre = "pepito"
    LET G_LISTA =  "/home/safre/CENVIO/", vnombre CLIPPED, ":",  HOY USING "YYYYMMDD"
   
   DECLARE c_contab CURSOR FOR
        SELECT codigo_cuenta, periodo_contab, fecha_transacc, tipo_registro,
	       monto, debito_credito, tipo_diario, referencia,
	       desc_conc_mov, codigo_analisis1, codigo_analisis2,
	       codigo_analisis3, codigo_analisis4
	FROM contab_pro_plano 
        START REPORT contab to "ingresos_promo.out"
   FOREACH c_contab INTO gr_report.*
        OUTPUT TO REPORT contab(gr_report.*)
   END FOREACH
        ERROR "GENERANDO REPORTE ARCHIVO PLANO"
        ATTRIBUTE (REVERSE)
        SLEEP 2

        ERROR " "
 
        PROMPT "       Proceso Finalizado Presione < ENTER > Para Continuar   "    
        ATTRIBUTE (REVERSE)
        FOR vresp
        ATTRIBUTE (REVERSE)

 END FUNCTION

 #########################################################################
    report contab(rpt)
 #########################################################################
      
    DEFINE rpt RECORD 
           codigo_cuenta       char(10),
           periodo_contab      char(7),
           fecha_transacc      date,
           tipo_registro       char(1),
           monto               decimal(16,2),
           debito_credito      char(1),
           tipo_diario         char(5),
           referencia          char(10),    ---char(21)
           desc_conc_mov       char(15),
           codigo_analisis1    char(15),
           codigo_analisis2    char(15),
           codigo_analisis3    char(15),
           codigo_analisis4    char(15)
    END RECORD

    OUTPUT
        PAGE LENGTH 5000
        LEFT MARGIN 0
        TOP MARGIN 0

    FORMAT
        PAGE HEADER
     
      SKIP 0 LINES
  
      ON EVERY ROW 

            LET vtotal = rpt.monto USING "##############.##"
            LET vvtotal = vtotal[1,14], vtotal[16,17]

       PRINT
                   
            COLUMN 01, rpt.codigo_cuenta,      -- Account Code
            COLUMN 16, rpt.periodo_contab,     -- Accounting Period
            COLUMN 23, rpt.fecha_transacc USING "yyyymmdd", -- Transaction Date   
            COLUMN 31, "  ",                   -- Blank 
            COLUMN 33, rpt.tipo_registro,      -- Record Type
            COLUMN 34, "  ",                    -- SunBusiness Journal Number
            COLUMN 36, "     ",
            COLUMN 41, "     ",                 -- Line Number
            COLUMN 46, "  ",                    -- BlanK
            --COLUMN 48, rpt.monto USING "#################0",  -- Amount
            COLUMN 50, vvtotal,                 -- Amount
            COLUMN 66, rpt.debito_credito,      -- Debit/Credit Marker
            COLUMN 67, " ",                     -- Allocation Indicador
            COLUMN 68,  "TEST",                 -- Journal Type
            COLUMN 73,  "    ",                 -- Journal Source
            COLUMN 78,  rpt.referencia,         -- Transaction Reference 
            COLUMN 93,  rpt.desc_conc_mov,      -- Descripction
            COLUMN 242, "              ",       -- Analysis Code 0
            COLUMN 257, rpt.codigo_analisis1,   -- Analysis Code 1
            COLUMN 272, rpt.codigo_analisis3,   -- Analysis Code 2
            COLUMN 287, rpt.codigo_analisis2,   -- Analysis Code 3
            COLUMN 302, "      ",               -- Analysis Code 4
            COLUMN 317, "      "                -- Analysis Code 5
end report

 ########################################################################
     FUNCTION actualiza_contab_pro_plano()
 ########################################################################

           DELETE FROM contab_pro_plano
              WHERE codigo_cuenta = "666"

     END FUNCTION

