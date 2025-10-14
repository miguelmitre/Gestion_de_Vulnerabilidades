###############################################################################
#Proyecto          => AFORE (MEXICO)                                          #
#Propietario       => E.F.P                                                   #
#Programa CONM001  => CONSULTA DE REGISTROS CONTABLES                         #
#Sistema           => CON                                                     #
#Autor             => Armando Rodriguez Castroparedes                         #
#Fecha             => 7 mayo 2002                                             #
#Fecha act         => 19 marzo 2003                                           #
#Modificado        => Fernando Herrera Hernandez                              #
#Fecha act         => 26 agosto 2004                                          #
#                     Se integro la funci¢n para generar la poliza contable   #
#Modificado        => Eduardo J. Resendiz Medina   26-Oct-2005                #
#                     En funcion reversa(ahora se reversa el archivo generado)#
#                     En listado_4 el T7 ya cae en su posocion correcta (347) #
#                     08 Noviembre 2005 Genera archivo el 1er dia de les DPSJL#
#                     17 Enero 2006 Reversa archivo - Sumariza Cuentas        #
#                     18 Mayo 2006 envio a archivo el analisis_cod = auxiliar #
#                     Junio 2006 Traspaso Afore-Afore / cedente - receptora   #
#                     Mayo 2008   Agrupacion cuentas y Transfer.entre Siefores#
#                     Junio 2008  Afraga Traspaso ICEFA ISSSTE 00038 / 00045  #
###############################################################################

DATABASE safre_af
GLOBALS
    DEFINE imp_1101_ent,
           imp_1101_sal,
           imp_eni,
           imp_tot_1102,
           imp_tot_1102_2        DECIMAL(15,2)

    DEFINE enter                 CHAR(1),
           g_usuario             CHAR(8),
           aux_pausa             CHAR(01),
           G_LISTA               CHAR(200),
           G_TRADUCTOR           CHAR(200),
           HORA                  CHAR(08),
           HOY                   DATE,
           vfolio                INTEGER,
           rechazo               SMALLINT,
           g_afore               RECORD LIKE tab_afore_local.*,
           g_paramgrales         RECORD LIKE seg_modulo.*,
           contador              INTEGER
         
    DEFINE vfemision09           DATE     ----14 Junio 2006
    DEFINE vfemision10           DATE     --- 14 Junio 2006
    DEFINE vtot_09               decimal(15,2)
    DEFINE vtot_10               decimal(15,2)
    DEFINE vfracc_tot09_1_peso   decimal(5,2)
    DEFINE vfracc_tot10_1_peso   decimal(5,2)
    DEFINE vfracc_tot09_1_acc    decimal(5,2)
    DEFINE vfracc_tot10_1_acc    decimal(5,2)
    DEFINE vfracc_tot09_2_peso   decimal(5,2)
    DEFINE vfracc_tot10_2_peso   decimal(5,2)
    DEFINE vfracc_tot09_2_acc    decimal(5,2)
    DEFINE vfracc_tot10_2_acc    decimal(5,2)
    DEFINE vdif_net2             DECIMAL(15,2)
    DEFINE vdif_net1             DECIMAL(15,2)
    DEFINE vneto_fracc_peso_1    decimal(5,2)
    DEFINE vneto_fracc_peso_2    decimal(5,2)
    DEFINE vneto_fracc_acc_1     decimal(5,2)
    DEFINE vneto_fracc_acc_2     decimal(5,2)
    DEFINE ban_neteo             SMALLINT
    DEFINE vident09,videntsie09  SMALLINT
    DEFINE vident10,videntsie10  SMALLINT
    DEFINE vpesos_sie2_09        DECIMAL(15,2)
    DEFINE vpesos_sie1_09        DECIMAL(15,2)
    DEFINE vacc_sie2_09          DECIMAL(15,2)
    DEFINE vacc_sie1_09          DECIMAL(15,2)
    DEFINE vpesos_sie2_10        DECIMAL(15,2)
    DEFINE vpesos_sie1_10        DECIMAL(15,2)
    DEFINE vacc_sie2_10          DECIMAL(15,2)
    DEFINE vacc_sie1_10          DECIMAL(15,2)

    DEFINE ventero1 INTEGER
    DEFINE vneto1   DECIMAL(15,2)
    DEFINE vfracc1  DECIMAL(15,2)
    DEFINE ventero2 INTEGER
    DEFINE vneto2   DECIMAL(15,2)
    DEFINE vfracc2  DECIMAL(15,2)
    DEFINE neto_acc1 DECIMAL(15,2)
    DEFINE neto_acc2 DECIMAL(15,2)

    DEFINE vtotal_total_09       DECIMAL(15,2)         ---erm 30 Abril 2007
    DEFINE vtotal_total_10       DECIMAL(15,2)         ---erm 30 Abril 2007

--->5 sie
    DEFINE vfracc_tot09_3_peso   decimal(5,2)
    DEFINE vfracc_tot09_4_peso   decimal(5,2)
    DEFINE vfracc_tot09_5_peso   decimal(5,2)
    DEFINE vfracc_tot10_3_peso   decimal(5,2)
    DEFINE vfracc_tot10_4_peso   decimal(5,2)
    DEFINE vfracc_tot10_5_peso   decimal(5,2)
    DEFINE vfracc_tot09_3_acc    decimal(5,2)
    DEFINE vfracc_tot09_4_acc    decimal(5,2)
    DEFINE vfracc_tot09_5_acc    decimal(5,2)
    DEFINE vfracc_tot10_3_acc    decimal(5,2)
    DEFINE vfracc_tot10_4_acc    decimal(5,2)
    DEFINE vfracc_tot10_5_acc    decimal(5,2)
    DEFINE vdif_net3             DECIMAL(15,2)
    DEFINE vdif_net4             DECIMAL(15,2)
    DEFINE vdif_net5             DECIMAL(15,2)
    DEFINE vneto_fracc_peso_3    decimal(5,2)
    DEFINE vneto_fracc_peso_4    decimal(5,2)
    DEFINE vneto_fracc_peso_5    decimal(5,2)
    DEFINE vneto_fracc_acc_3     decimal(5,2)
    DEFINE vneto_fracc_acc_4     decimal(5,2)
    DEFINE vneto_fracc_acc_5     decimal(5,2)
    DEFINE vpesos_sie3_09        DECIMAL(15,2)
    DEFINE vpesos_sie4_09        DECIMAL(15,2)
    DEFINE vpesos_sie5_09        DECIMAL(15,2)
    DEFINE vacc_sie3_09          DECIMAL(15,2)
    DEFINE vacc_sie4_09          DECIMAL(15,2)
    DEFINE vacc_sie5_09          DECIMAL(15,2)
    DEFINE vpesos_sie3_10        DECIMAL(15,2)
    DEFINE vpesos_sie4_10        DECIMAL(15,2)
    DEFINE vpesos_sie5_10        DECIMAL(15,2)
    DEFINE vacc_sie3_10          DECIMAL(15,2)
    DEFINE vacc_sie4_10          DECIMAL(15,2)
    DEFINE vacc_sie5_10          DECIMAL(15,2)
    DEFINE ventero3 INTEGER
    DEFINE vneto3   DECIMAL(15,2)
    DEFINE vfracc3  DECIMAL(15,2)
    DEFINE ventero4 INTEGER
    DEFINE vneto4   DECIMAL(15,2)
    DEFINE vfracc4  DECIMAL(15,2)
    DEFINE ventero5 INTEGER
    DEFINE vneto5   DECIMAL(15,2)
    DEFINE vfracc5  DECIMAL(15,2)
    DEFINE neto_acc3 DECIMAL(15,2)
    DEFINE neto_acc4 DECIMAL(15,2)
    DEFINE neto_acc5 DECIMAL(15,2)
    DEFINE ban_cumple SMALLINT
---<5 sie
    DEFINE  vfecha_reporte DATE

    DEFINE vfemision39     DATE
    DEFINE vimporte_21315_sie1 DECIMAL(15,2)
    DEFINE vimporte_21315_sie2 DECIMAL(15,2)
    DEFINE vimporte_21315_sie3 DECIMAL(15,2)
    DEFINE vimporte_21315_sie4 DECIMAL(15,2)
    DEFINE vimporte_21315_sie5 DECIMAL(15,2)
    DEFINE vimporte_22332_sie1 DECIMAL(15,2)
    DEFINE vimporte_22332_sie2 DECIMAL(15,2)
    DEFINE vimporte_22332_sie3 DECIMAL(15,2)
    DEFINE vimporte_22332_sie4 DECIMAL(15,2)
    DEFINE vimporte_22332_sie5 DECIMAL(15,2)
    DEFINE vimporte_23351_sie1 DECIMAL(15,2)
    DEFINE vimporte_23351_sie2 DECIMAL(15,2)
    DEFINE vimporte_23351_sie3 DECIMAL(15,2)
    DEFINE vimporte_23351_sie4 DECIMAL(15,2)
    DEFINE vimporte_23351_sie5 DECIMAL(15,2)
    DEFINE vimporte_28351_sie1 DECIMAL(15,2)
    DEFINE vimporte_28351_sie2 DECIMAL(15,2)
    DEFINE vimporte_28351_sie3 DECIMAL(15,2)
    DEFINE vimporte_28351_sie4 DECIMAL(15,2)
    DEFINE vimporte_28351_sie5 DECIMAL(15,2)
    DEFINE vimporte_21334_sie1 DECIMAL(15,2)
    DEFINE vimporte_21334_sie2 DECIMAL(15,2)
    DEFINE vimporte_21334_sie3 DECIMAL(15,2)
    DEFINE vimporte_21334_sie4 DECIMAL(15,2)
    DEFINE vimporte_21334_sie5 DECIMAL(15,2)
    DEFINE vimporte_22334_sie1 DECIMAL(15,2)
    DEFINE vimporte_22334_sie2 DECIMAL(15,2)
    DEFINE vimporte_22334_sie3 DECIMAL(15,2)
    DEFINE vimporte_22334_sie4 DECIMAL(15,2)
    DEFINE vimporte_22334_sie5 DECIMAL(15,2)
    DEFINE vimporte_23334_sie1 DECIMAL(15,2)
    DEFINE vimporte_23334_sie2 DECIMAL(15,2)
    DEFINE vimporte_23334_sie3 DECIMAL(15,2)
    DEFINE vimporte_23334_sie4 DECIMAL(15,2)
    DEFINE vimporte_23334_sie5 DECIMAL(15,2)
    DEFINE vimporte_28011_sie1 DECIMAL(15,2)
    DEFINE vimporte_28011_sie2 DECIMAL(15,2)
    DEFINE vimporte_28011_sie3 DECIMAL(15,2)
    DEFINE vimporte_28011_sie4 DECIMAL(15,2)
    DEFINE vimporte_28011_sie5 DECIMAL(15,2)
    DEFINE vimporte_29024_sie1 DECIMAL(15,2)
    DEFINE vimporte_29024_sie2 DECIMAL(15,2)
    DEFINE vimporte_29024_sie3 DECIMAL(15,2)
    DEFINE vimporte_29024_sie4 DECIMAL(15,2)
    DEFINE vimporte_29024_sie5 DECIMAL(15,2)
    DEFINE vimporte_29124_sie1 DECIMAL(15,2) 
    DEFINE vimporte_29124_sie2 DECIMAL(15,2) 
    DEFINE vimporte_29124_sie3 DECIMAL(15,2) 
    DEFINE vimporte_29124_sie4 DECIMAL(15,2) 
    DEFINE vimporte_29124_sie5 DECIMAL(15,2) 
    DEFINE vimporte_29027_sie1 DECIMAL(15,2)
    DEFINE vimporte_29027_sie2 DECIMAL(15,2)
    DEFINE vimporte_29027_sie3 DECIMAL(15,2)
    DEFINE vimporte_29027_sie4 DECIMAL(15,2)
    DEFINE vimporte_29027_sie5 DECIMAL(15,2)
    DEFINE vimporte_29127_sie1 DECIMAL(15,2) 
    DEFINE vimporte_29127_sie2 DECIMAL(15,2) 
    DEFINE vimporte_29127_sie3 DECIMAL(15,2) 
    DEFINE vimporte_29127_sie4 DECIMAL(15,2) 
    DEFINE vimporte_29127_sie5 DECIMAL(15,2) 
    DEFINE vimporte_29028_sie1 DECIMAL(15,2)
    DEFINE vimporte_29028_sie2 DECIMAL(15,2)
    DEFINE vimporte_29028_sie3 DECIMAL(15,2)
    DEFINE vimporte_29028_sie4 DECIMAL(15,2)
    DEFINE vimporte_29028_sie5 DECIMAL(15,2)
    DEFINE vimporte_29128_sie1 DECIMAL(15,2) 
    DEFINE vimporte_29128_sie2 DECIMAL(15,2) 
    DEFINE vimporte_29128_sie3 DECIMAL(15,2) 
    DEFINE vimporte_29128_sie4 DECIMAL(15,2) 
    DEFINE vimporte_29128_sie5 DECIMAL(15,2) 


    DEFINE vimporte_21315_sie1_pos DECIMAL(15,2)
    DEFINE vimporte_21315_sie2_pos DECIMAL(15,2)
    DEFINE vimporte_21315_sie3_pos DECIMAL(15,2)
    DEFINE vimporte_21315_sie4_pos DECIMAL(15,2)
    DEFINE vimporte_21315_sie5_pos DECIMAL(15,2)
    DEFINE vimporte_22332_sie1_pos DECIMAL(15,2)
    DEFINE vimporte_22332_sie2_pos DECIMAL(15,2)
    DEFINE vimporte_22332_sie3_pos DECIMAL(15,2)
    DEFINE vimporte_22332_sie4_pos DECIMAL(15,2)
    DEFINE vimporte_22332_sie5_pos DECIMAL(15,2)
    DEFINE vimporte_23351_sie1_pos DECIMAL(15,2)
    DEFINE vimporte_23351_sie2_pos DECIMAL(15,2)
    DEFINE vimporte_23351_sie3_pos DECIMAL(15,2)
    DEFINE vimporte_23351_sie4_pos DECIMAL(15,2)
    DEFINE vimporte_23351_sie5_pos DECIMAL(15,2)
    DEFINE vimporte_28351_sie1_pos DECIMAL(15,2)
    DEFINE vimporte_28351_sie2_pos DECIMAL(15,2)
    DEFINE vimporte_28351_sie3_pos DECIMAL(15,2)
    DEFINE vimporte_28351_sie4_pos DECIMAL(15,2)
    DEFINE vimporte_28351_sie5_pos DECIMAL(15,2)
    DEFINE vimporte_21334_sie1_pos DECIMAL(15,2)
    DEFINE vimporte_21334_sie2_pos DECIMAL(15,2)
    DEFINE vimporte_21334_sie3_pos DECIMAL(15,2)
    DEFINE vimporte_21334_sie4_pos DECIMAL(15,2)
    DEFINE vimporte_21334_sie5_pos DECIMAL(15,2)
    DEFINE vimporte_22334_sie1_pos DECIMAL(15,2)
    DEFINE vimporte_22334_sie2_pos DECIMAL(15,2)
    DEFINE vimporte_22334_sie3_pos DECIMAL(15,2)
    DEFINE vimporte_22334_sie4_pos DECIMAL(15,2)
    DEFINE vimporte_22334_sie5_pos DECIMAL(15,2)
    DEFINE vimporte_23334_sie1_pos DECIMAL(15,2)
    DEFINE vimporte_23334_sie2_pos DECIMAL(15,2)
    DEFINE vimporte_23334_sie3_pos DECIMAL(15,2)
    DEFINE vimporte_23334_sie4_pos DECIMAL(15,2)
    DEFINE vimporte_23334_sie5_pos DECIMAL(15,2)
    DEFINE vimporte_28011_sie1_pos DECIMAL(15,2)
    DEFINE vimporte_28011_sie2_pos DECIMAL(15,2)
    DEFINE vimporte_28011_sie3_pos DECIMAL(15,2)
    DEFINE vimporte_28011_sie4_pos DECIMAL(15,2)
    DEFINE vimporte_28011_sie5_pos DECIMAL(15,2)

    DEFINE vimporte_29024_sie1_pos DECIMAL(15,2)
    DEFINE vimporte_29024_sie2_pos DECIMAL(15,2)
    DEFINE vimporte_29024_sie3_pos DECIMAL(15,2)
    DEFINE vimporte_29024_sie4_pos DECIMAL(15,2)
    DEFINE vimporte_29024_sie5_pos DECIMAL(15,2)
    DEFINE vimporte_29124_sie1_pos DECIMAL(15,2) 
    DEFINE vimporte_29124_sie2_pos DECIMAL(15,2) 
    DEFINE vimporte_29124_sie3_pos DECIMAL(15,2) 
    DEFINE vimporte_29124_sie4_pos DECIMAL(15,2) 
    DEFINE vimporte_29124_sie5_pos DECIMAL(15,2) 
    DEFINE vimporte_29027_sie1_pos DECIMAL(15,2)
    DEFINE vimporte_29027_sie2_pos DECIMAL(15,2)
    DEFINE vimporte_29027_sie3_pos DECIMAL(15,2)
    DEFINE vimporte_29027_sie4_pos DECIMAL(15,2)
    DEFINE vimporte_29027_sie5_pos DECIMAL(15,2)
    DEFINE vimporte_29127_sie1_pos DECIMAL(15,2) 
    DEFINE vimporte_29127_sie2_pos DECIMAL(15,2) 
    DEFINE vimporte_29127_sie3_pos DECIMAL(15,2) 
    DEFINE vimporte_29127_sie4_pos DECIMAL(15,2) 
    DEFINE vimporte_29127_sie5_pos DECIMAL(15,2) 
    DEFINE vimporte_29028_sie1_pos DECIMAL(15,2)
    DEFINE vimporte_29028_sie2_pos DECIMAL(15,2)
    DEFINE vimporte_29028_sie3_pos DECIMAL(15,2)
    DEFINE vimporte_29028_sie4_pos DECIMAL(15,2)
    DEFINE vimporte_29028_sie5_pos DECIMAL(15,2)
    DEFINE vimporte_29128_sie1_pos DECIMAL(15,2) 
    DEFINE vimporte_29128_sie2_pos DECIMAL(15,2) 
    DEFINE vimporte_29128_sie3_pos DECIMAL(15,2) 
    DEFINE vimporte_29128_sie4_pos DECIMAL(15,2) 
    DEFINE vimporte_29128_sie5_pos DECIMAL(15,2) 

    DEFINE vimp_1102_21315_sie1,
           vimp_1102_21315_sie2,
           vimp_1102_21315_sie3,
           vimp_1102_21315_sie4,
           vimp_1102_21315_sie5,
           vimp_1102_22332_sie1,
           vimp_1102_22332_sie2,
           vimp_1102_22332_sie3,
           vimp_1102_22332_sie4,
           vimp_1102_22332_sie5,
           vimp_1102_23351_sie1,
           vimp_1102_23351_sie2,
           vimp_1102_23351_sie3,
           vimp_1102_23351_sie4,
           vimp_1102_23351_sie5,
           vimp_1102_28351_sie1,
           vimp_1102_28351_sie2,
           vimp_1102_28351_sie3,
           vimp_1102_28351_sie4,
           vimp_1102_28351_sie5,
           vimp_1102_29124_sie1,
           vimp_1102_29124_sie2,
           vimp_1102_29124_sie3,
           vimp_1102_29124_sie4,
           vimp_1102_29124_sie5,
           vimp_1102_29127_sie1,
           vimp_1102_29127_sie2,
           vimp_1102_29127_sie3,
           vimp_1102_29127_sie4,
           vimp_1102_29127_sie5,
           vimp_1102_29128_sie1,
           vimp_1102_29128_sie2,
           vimp_1102_29128_sie3,
           vimp_1102_29128_sie4,
           vimp_1102_29128_sie5,

           vimp_1102_21315_2sie1,
           vimp_1102_21315_2sie2,
           vimp_1102_21315_2sie3,
           vimp_1102_21315_2sie4,
           vimp_1102_21315_2sie5,
           vimp_1102_22332_2sie1,
           vimp_1102_22332_2sie2,
           vimp_1102_22332_2sie3,
           vimp_1102_22332_2sie4,
           vimp_1102_22332_2sie5,
           vimp_1102_23351_2sie1,
           vimp_1102_23351_2sie2,
           vimp_1102_23351_2sie3,
           vimp_1102_23351_2sie4,
           vimp_1102_23351_2sie5,
           vimp_1102_28351_2sie1,
           vimp_1102_28351_2sie2,
           vimp_1102_28351_2sie3,
           vimp_1102_28351_2sie4,
           vimp_1102_28351_2sie5,
           vimp_1102_29124_2sie1,
           vimp_1102_29124_2sie2,
           vimp_1102_29124_2sie3,
           vimp_1102_29124_2sie4,
           vimp_1102_29124_2sie5,
           vimp_1102_29127_2sie1,
           vimp_1102_29127_2sie2,
           vimp_1102_29127_2sie3,
           vimp_1102_29127_2sie4,
           vimp_1102_29127_2sie5,
           vimp_1102_29128_2sie1,
           vimp_1102_29128_2sie2,
           vimp_1102_29128_2sie3,
           vimp_1102_29128_2sie4,
           vimp_1102_29128_2sie5,

           vimp_1102_21334_sie1,
           vimp_1102_21334_sie2,
           vimp_1102_21334_sie3,
           vimp_1102_21334_sie4,
           vimp_1102_21334_sie5,
           vimp_1102_22334_sie1,
           vimp_1102_22334_sie2,
           vimp_1102_22334_sie3,
           vimp_1102_22334_sie4,
           vimp_1102_22334_sie5,
           vimp_1102_23334_sie1,
           vimp_1102_23334_sie2,
           vimp_1102_23334_sie3,
           vimp_1102_23334_sie4,
           vimp_1102_23334_sie5,
           vimp_1102_28011_sie1,
           vimp_1102_28011_sie2,
           vimp_1102_28011_sie3,
           vimp_1102_28011_sie4,
           vimp_1102_28011_sie5,
           vimp_1102_29024_sie1,
           vimp_1102_29024_sie2,
           vimp_1102_29024_sie3,
           vimp_1102_29024_sie4,
           vimp_1102_29024_sie5,
           vimp_1102_29027_sie1,
           vimp_1102_29027_sie2,
           vimp_1102_29027_sie3,
           vimp_1102_29027_sie4,
           vimp_1102_29027_sie5,
           vimp_1102_29028_sie1,
           vimp_1102_29028_sie2,
           vimp_1102_29028_sie3,
           vimp_1102_29028_sie4,
           vimp_1102_29028_sie5 DECIMAL (15,2)


    DEFINE ban21315_sie1 SMALLINT
    DEFINE ban21315_sie2 SMALLINT
    DEFINE ban21315_sie3 SMALLINT
    DEFINE ban21315_sie4 SMALLINT
    DEFINE ban21315_sie5 SMALLINT
    DEFINE ban22332_sie1 SMALLINT
    DEFINE ban22332_sie2 SMALLINT
    DEFINE ban22332_sie3 SMALLINT
    DEFINE ban22332_sie4 SMALLINT
    DEFINE ban22332_sie5 SMALLINT
    DEFINE ban23351_sie1 SMALLINT
    DEFINE ban23351_sie2 SMALLINT
    DEFINE ban23351_sie3 SMALLINT
    DEFINE ban23351_sie4 SMALLINT
    DEFINE ban23351_sie5 SMALLINT
    DEFINE ban28351_sie1 SMALLINT
    DEFINE ban28351_sie2 SMALLINT
    DEFINE ban28351_sie3 SMALLINT
    DEFINE ban28351_sie4 SMALLINT
    DEFINE ban28351_sie5 SMALLINT
    DEFINE ban29124_sie1 SMALLINT
    DEFINE ban29124_sie2 SMALLINT
    DEFINE ban29124_sie3 SMALLINT
    DEFINE ban29124_sie4 SMALLINT
    DEFINE ban29124_sie5 SMALLINT
    DEFINE ban29127_sie1 SMALLINT
    DEFINE ban29127_sie2 SMALLINT
    DEFINE ban29127_sie3 SMALLINT
    DEFINE ban29127_sie4 SMALLINT
    DEFINE ban29127_sie5 SMALLINT
    DEFINE ban29128_sie1 SMALLINT
    DEFINE ban29128_sie2 SMALLINT
    DEFINE ban29128_sie3 SMALLINT
    DEFINE ban29128_sie4 SMALLINT
    DEFINE ban29128_sie5 SMALLINT

    DEFINE ban_00039     SMALLINT
    DEFINE importe2101_39   DECIMAL(15,2)
    DEFINE importe2101_39_1 DECIMAL(15,2)
    DEFINE importe2101_39_2 DECIMAL(15,2)
    DEFINE importe2101_39_3 DECIMAL(15,2)
    DEFINE importe2101_39_4 DECIMAL(15,2)
    DEFINE importe2101_39_5 DECIMAL(15,2)

END GLOBALS

MAIN

    OPTIONS PROMPT LINE LAST,
    INPUT WRAP,
    COMMENT LINE LAST
    DEFER INTERRUPT

    CALL inicio()   #i
    CALL STARTLOG("CONM001.log")
    CALL proceso_principal()   #pp

END MAIN

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "CONM0011" ATTRIBUTE(BORDER)
    DISPLAY " CONM001            CONSULTA DE REGISTROS CONTABLES                               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

    MENU "CONSULTA "
        COMMAND "Registros" "Consulta Registros contables "
	        CALL consulta()
	        CLEAR FORM
       
        COMMAND "Genera archivo" "Genera archivo contable del dia"
                CALL calcula_fracc_trasp()
                CALL genera_reporte()
	        CLEAR FORM

        COMMAND KEY(V) "reVersa" "Reversa procesos por dia"
                CALL reversa()
	        CLEAR FORM

        COMMAND "Salir" "Salir de Programa"
                EXIT MENU
    END MENU

END FUNCTION

FUNCTION inicio()
#i---------------

    LET rechazo = 0
    LET HOY = TODAY

    LET HORA = TIME

    SELECT *, USER 
    INTO   g_afore.*, g_usuario 
    FROM   tab_afore_local

END FUNCTION

--->erm 11 Sep 2008 trnasie
FUNCTION neteo_trans_sie()

   DEFINE vsie_sie     SMALLINT
   DEFINE vimporte     DECIMAL(15,2)
   DEFINE vimporte_sie DECIMAL(15,2)
   DEFINE vtran_sie    INTEGER
   DEFINE folio39      INTEGER
   DEFINE sief1,sief2,
         sief3,sief4,
         sief5,i       SMALLINT
   DEFINE cadena       CHAR(35)

   LET vimporte_21315_sie1 = 0
   LET vimporte_21315_sie2 = 0
   LET vimporte_21315_sie3 = 0
   LET vimporte_21315_sie4 = 0
   LET vimporte_21315_sie5 = 0
   LET vimporte_22332_sie1 = 0
   LET vimporte_22332_sie2 = 0
   LET vimporte_22332_sie3 = 0
   LET vimporte_22332_sie4 = 0
   LET vimporte_22332_sie5 = 0
   LET vimporte_23351_sie1 = 0
   LET vimporte_23351_sie2 = 0
   LET vimporte_23351_sie3 = 0
   LET vimporte_23351_sie4 = 0
   LET vimporte_23351_sie5 = 0
   LET vimporte_28351_sie1 = 0
   LET vimporte_28351_sie2 = 0
   LET vimporte_28351_sie3 = 0
   LET vimporte_28351_sie4 = 0
   LET vimporte_28351_sie5 = 0
   LET vimporte_21334_sie1 = 0
   LET vimporte_21334_sie2 = 0
   LET vimporte_21334_sie3 = 0
   LET vimporte_21334_sie4 = 0
   LET vimporte_21334_sie5 = 0
   LET vimporte_22334_sie1 = 0
   LET vimporte_22334_sie2 = 0
   LET vimporte_22334_sie3 = 0
   LET vimporte_22334_sie4 = 0
   LET vimporte_22334_sie5 = 0
   LET vimporte_23334_sie1 = 0
   LET vimporte_23334_sie2 = 0
   LET vimporte_23334_sie3 = 0
   LET vimporte_23334_sie4 = 0
   LET vimporte_23334_sie5 = 0
   LET vimporte_28011_sie1 = 0
   LET vimporte_28011_sie2 = 0
   LET vimporte_28011_sie3 = 0
   LET vimporte_28011_sie4 = 0
   LET vimporte_28011_sie5 = 0
   LET vimporte_29024_sie1 = 0
   LET vimporte_29024_sie2 = 0
   LET vimporte_29024_sie3 = 0
   LET vimporte_29024_sie4 = 0
   LET vimporte_29024_sie5 = 0
   LET vimporte_29124_sie1 = 0
   LET vimporte_29124_sie2 = 0
   LET vimporte_29124_sie3 = 0
   LET vimporte_29124_sie4 = 0
   LET vimporte_29124_sie5 = 0
   LET vimporte_29027_sie1 = 0
   LET vimporte_29027_sie2 = 0
   LET vimporte_29027_sie3 = 0
   LET vimporte_29027_sie4 = 0
   LET vimporte_29027_sie5 = 0
   LET vimporte_29127_sie1 = 0
   LET vimporte_29127_sie2 = 0
   LET vimporte_29127_sie3 = 0
   LET vimporte_29127_sie4 = 0
   LET vimporte_29127_sie5 = 0
   LET vimporte_29028_sie1 = 0
   LET vimporte_29028_sie2 = 0
   LET vimporte_29028_sie3 = 0
   LET vimporte_29028_sie4 = 0
   LET vimporte_29028_sie5 = 0
   LET vimporte_29128_sie1 = 0
   LET vimporte_29128_sie2 = 0
   LET vimporte_29128_sie3 = 0
   LET vimporte_29128_sie4 = 0
   LET vimporte_29128_sie5 = 0

   LET vimporte_21315_sie1_pos = 0
   LET vimporte_21315_sie2_pos = 0
   LET vimporte_21315_sie3_pos = 0
   LET vimporte_21315_sie4_pos = 0
   LET vimporte_21315_sie5_pos = 0
   LET vimporte_22332_sie1_pos = 0
   LET vimporte_22332_sie2_pos = 0
   LET vimporte_22332_sie3_pos = 0
   LET vimporte_22332_sie4_pos = 0
   LET vimporte_22332_sie5_pos = 0
   LET vimporte_23351_sie1_pos = 0
   LET vimporte_23351_sie2_pos = 0
   LET vimporte_23351_sie3_pos = 0
   LET vimporte_23351_sie4_pos = 0
   LET vimporte_23351_sie5_pos = 0
   LET vimporte_28351_sie1_pos = 0
   LET vimporte_28351_sie2_pos = 0
   LET vimporte_28351_sie3_pos = 0
   LET vimporte_28351_sie4_pos = 0
   LET vimporte_28351_sie5_pos = 0
   LET vimporte_21334_sie1_pos = 0
   LET vimporte_21334_sie2_pos = 0
   LET vimporte_21334_sie3_pos = 0
   LET vimporte_21334_sie4_pos = 0
   LET vimporte_21334_sie5_pos = 0
   LET vimporte_22334_sie1_pos = 0
   LET vimporte_22334_sie2_pos = 0
   LET vimporte_22334_sie3_pos = 0
   LET vimporte_22334_sie4_pos = 0
   LET vimporte_22334_sie5_pos = 0
   LET vimporte_23334_sie1_pos = 0
   LET vimporte_23334_sie2_pos = 0
   LET vimporte_23334_sie3_pos = 0
   LET vimporte_23334_sie4_pos = 0
   LET vimporte_23334_sie5_pos = 0
   LET vimporte_28011_sie1_pos = 0
   LET vimporte_28011_sie2_pos = 0
   LET vimporte_28011_sie3_pos = 0
   LET vimporte_28011_sie4_pos = 0
   LET vimporte_28011_sie5_pos = 0
   LET vimporte_29024_sie1_pos = 0
   LET vimporte_29024_sie2_pos = 0
   LET vimporte_29024_sie3_pos = 0
   LET vimporte_29024_sie4_pos = 0
   LET vimporte_29024_sie5_pos = 0
   LET vimporte_29124_sie1_pos = 0
   LET vimporte_29124_sie2_pos = 0
   LET vimporte_29124_sie3_pos = 0
   LET vimporte_29124_sie4_pos = 0
   LET vimporte_29124_sie5_pos = 0
   LET vimporte_29027_sie1_pos = 0
   LET vimporte_29027_sie2_pos = 0
   LET vimporte_29027_sie3_pos = 0
   LET vimporte_29027_sie4_pos = 0
   LET vimporte_29027_sie5_pos = 0
   LET vimporte_29127_sie1_pos = 0
   LET vimporte_29127_sie2_pos = 0
   LET vimporte_29127_sie3_pos = 0
   LET vimporte_29127_sie4_pos = 0
   LET vimporte_29127_sie5_pos = 0
   LET vimporte_29028_sie1_pos = 0
   LET vimporte_29028_sie2_pos = 0
   LET vimporte_29028_sie3_pos = 0
   LET vimporte_29028_sie4_pos = 0
   LET vimporte_29028_sie5_pos = 0
   LET vimporte_29128_sie1_pos = 0
   LET vimporte_29128_sie2_pos = 0
   LET vimporte_29128_sie3_pos = 0
   LET vimporte_29128_sie4_pos = 0
   LET vimporte_29128_sie5_pos = 0

   LET importe2101_39_1        = 0
   LET importe2101_39_2        = 0
   LET importe2101_39_3        = 0
   LET importe2101_39_4        = 0
   LET importe2101_39_5        = 0

   LET vimp_1102_21315_sie1 = 0
   LET vimp_1102_21315_sie2 = 0
   LET vimp_1102_21315_sie3 = 0
   LET vimp_1102_21315_sie4 = 0
   LET vimp_1102_21315_sie5 = 0
   LET vimp_1102_22332_sie1 = 0
   LET vimp_1102_22332_sie2 = 0
   LET vimp_1102_22332_sie3 = 0
   LET vimp_1102_22332_sie4 = 0
   LET vimp_1102_22332_sie5 = 0
   LET vimp_1102_23351_sie1 = 0
   LET vimp_1102_23351_sie2 = 0
   LET vimp_1102_23351_sie3 = 0
   LET vimp_1102_23351_sie4 = 0
   LET vimp_1102_23351_sie5 = 0
   LET vimp_1102_28351_sie1 = 0
   LET vimp_1102_28351_sie2 = 0
   LET vimp_1102_28351_sie3 = 0
   LET vimp_1102_28351_sie4 = 0
   LET vimp_1102_28351_sie5 = 0
   LET vimp_1102_29124_sie1 = 0
   LET vimp_1102_29124_sie2 = 0
   LET vimp_1102_29124_sie3 = 0
   LET vimp_1102_29124_sie4 = 0
   LET vimp_1102_29124_sie5 = 0
   LET vimp_1102_29127_sie1 = 0
   LET vimp_1102_29127_sie2 = 0
   LET vimp_1102_29127_sie3 = 0
   LET vimp_1102_29127_sie4 = 0
   LET vimp_1102_29127_sie5 = 0
   LET vimp_1102_29128_sie1 = 0
   LET vimp_1102_29128_sie2 = 0
   LET vimp_1102_29128_sie3 = 0
   LET vimp_1102_29128_sie4 = 0
   LET vimp_1102_29128_sie5 = 0

   LET vimp_1102_21334_sie1 = 0
   LET vimp_1102_21334_sie2 = 0
   LET vimp_1102_21334_sie3 = 0
   LET vimp_1102_21334_sie4 = 0
   LET vimp_1102_21334_sie5 = 0
   LET vimp_1102_22334_sie1 = 0
   LET vimp_1102_22334_sie2 = 0
   LET vimp_1102_22334_sie3 = 0
   LET vimp_1102_22334_sie4 = 0
   LET vimp_1102_22334_sie5 = 0
   LET vimp_1102_23334_sie1 = 0
   LET vimp_1102_23334_sie2 = 0
   LET vimp_1102_23334_sie3 = 0
   LET vimp_1102_23334_sie4 = 0
   LET vimp_1102_23334_sie5 = 0
   LET vimp_1102_28011_sie1 = 0
   LET vimp_1102_28011_sie2 = 0
   LET vimp_1102_28011_sie3 = 0
   LET vimp_1102_28011_sie4 = 0
   LET vimp_1102_28011_sie5 = 0
   LET vimp_1102_29024_sie1 = 0
   LET vimp_1102_29024_sie2 = 0
   LET vimp_1102_29024_sie3 = 0
   LET vimp_1102_29024_sie4 = 0
   LET vimp_1102_29024_sie5 = 0
   LET vimp_1102_29027_sie1 = 0
   LET vimp_1102_29027_sie2 = 0
   LET vimp_1102_29027_sie3 = 0
   LET vimp_1102_29027_sie4 = 0
   LET vimp_1102_29027_sie5 = 0
   LET vimp_1102_29028_sie1 = 0
   LET vimp_1102_29028_sie2 = 0
   LET vimp_1102_29028_sie3 = 0
   LET vimp_1102_29028_sie4 = 0
   LET vimp_1102_29028_sie5 = 0

   LET vimp_1102_21315_2sie1 = 0
   LET vimp_1102_21315_2sie2 = 0
   LET vimp_1102_21315_2sie3 = 0
   LET vimp_1102_21315_2sie4 = 0
   LET vimp_1102_21315_2sie5 = 0
   LET vimp_1102_22332_2sie1 = 0
   LET vimp_1102_22332_2sie2 = 0
   LET vimp_1102_22332_2sie3 = 0
   LET vimp_1102_22332_2sie4 = 0
   LET vimp_1102_22332_2sie5 = 0
   LET vimp_1102_23351_2sie1 = 0
   LET vimp_1102_23351_2sie2 = 0
   LET vimp_1102_23351_2sie3 = 0
   LET vimp_1102_23351_2sie4 = 0
   LET vimp_1102_23351_2sie5 = 0
   LET vimp_1102_28351_2sie1 = 0
   LET vimp_1102_28351_2sie2 = 0
   LET vimp_1102_28351_2sie3 = 0
   LET vimp_1102_28351_2sie4 = 0
   LET vimp_1102_28351_2sie5 = 0
   LET vimp_1102_29124_2sie1 = 0
   LET vimp_1102_29124_2sie2 = 0
   LET vimp_1102_29124_2sie3 = 0
   LET vimp_1102_29124_2sie4 = 0
   LET vimp_1102_29124_2sie5 = 0
   LET vimp_1102_29127_2sie1 = 0
   LET vimp_1102_29127_2sie2 = 0
   LET vimp_1102_29127_2sie3 = 0
   LET vimp_1102_29127_2sie4 = 0
   LET vimp_1102_29127_2sie5 = 0
   LET vimp_1102_29128_2sie1 = 0
   LET vimp_1102_29128_2sie2 = 0
   LET vimp_1102_29128_2sie3 = 0
   LET vimp_1102_29128_2sie4 = 0
   LET vimp_1102_29128_2sie5 = 0



   LET vimporte  = 0
   LET vtran_sie = 0
   LET vsie_sie  = 0
   LET sief1     = 0
   LET sief2     = 0
   LET sief3     = 0
   LET sief4     = 0
   LET sief5     = 0
   LET importe2101_39 = 0
   LET folio39   = 0
   LET ban_00039 = 0

{   SELECT 'X'
   FROM  con_transaccion
   WHERE proceso_cod   = '00039'
   AND   fecha_emision = vfecha_reporte
}

   SELECT MAX(fecha_emision),MAX(folio)
   INTO   vfemision39,folio39
   FROM   con_transaccion
   WHERE  proceso_cod = '00039'
   AND    estado      = 20
   --GROUP BY 2

   IF vfemision39 = vfecha_reporte THEN       -----comentar al momento de quitar el comentario
   --IF STATUS = 0 THEN
      DECLARE cneteo_sie CURSOR FOR 
         SELECT importe,transaccion_cod,siefore
         FROM   con_transaccion
         WHERE  proceso_cod   = '00039'
         AND    fecha_emision = vfecha_reporte
         --AND    folio = folio39
         AND    identificador = 2
         ORDER BY 2,3

      FOREACH cneteo_sie INTO vimporte_sie,vtran_sie,vsie_sie

#salidas
#subcuenta 1 retiro 
         CASE vtran_sie
            WHEN 21315
               IF vsie_sie  = 1 THEN
                   LET vimporte_21315_sie1 = vimporte_sie
                   IF  vimporte_21315_sie1 IS NULL THEN LET vimporte_21315_sie1 = 0
                   END IF
               END IF

               IF vsie_sie  = 2 THEN
                   LET vimporte_21315_sie2 = vimporte_sie
                   IF  vimporte_21315_sie2 IS NULL THEN LET vimporte_21315_sie2 = 0
                   END IF
               END IF

               IF vsie_sie  = 3 THEN
                   LET vimporte_21315_sie3 = vimporte_sie
                   IF  vimporte_21315_sie3 IS NULL THEN LET vimporte_21315_sie3 = 0
                   END IF
               END IF

               IF vsie_sie  = 4 THEN
                   LET vimporte_21315_sie4 = vimporte_sie
                   IF  vimporte_21315_sie4 IS NULL THEN LET vimporte_21315_sie4 = 0
                   END IF
               END IF

               IF vsie_sie  = 5 THEN
                   LET vimporte_21315_sie5 = vimporte_sie
                   IF  vimporte_21315_sie5 IS NULL THEN LET vimporte_21315_sie5 = 0
                   END IF
               END IF

#subcuenta 2,6,9 cv, estatal, especial
            WHEN 22332
               IF vsie_sie  = 1 THEN
                    LET vimporte_22332_sie1 = vimporte_sie
                    IF  vimporte_22332_sie1 IS NULL THEN LET vimporte_22332_sie1 = 0
                    END IF
               END IF

               IF vsie_sie  = 2 THEN
                    LET vimporte_22332_sie2 = vimporte_sie
                    IF  vimporte_22332_sie2 IS NULL THEN LET vimporte_22332_sie2 = 0
                    END IF
               END IF

               IF vsie_sie  = 3 THEN
                    LET vimporte_22332_sie3 = vimporte_sie
                    IF  vimporte_22332_sie3 IS NULL THEN LET vimporte_22332_sie3 = 0
                    END IF
               END IF

               IF vsie_sie  = 4 THEN
                    LET vimporte_22332_sie4 = vimporte_sie
                    IF  vimporte_22332_sie4 IS NULL THEN LET vimporte_22332_sie4 = 0
                    END IF
               END IF

               IF vsie_sie  = 5 THEN
                    LET vimporte_22332_sie5 = vimporte_sie
                    IF  vimporte_22332_sie5 IS NULL THEN LET vimporte_22332_sie5 = 0
                    END IF
               END IF

#cuota social
            WHEN 23351
               IF vsie_sie  = 1 THEN
                    LET vimporte_23351_sie1 = vimporte_sie
                    IF  vimporte_23351_sie1 IS NULL THEN LET vimporte_23351_sie1 = 0
                    END IF
               END IF

               IF vsie_sie  = 2 THEN
                    LET vimporte_23351_sie2 = vimporte_sie
                    IF  vimporte_23351_sie2 IS NULL THEN LET vimporte_23351_sie2 = 0
                    END IF
               END IF

               IF vsie_sie  = 3 THEN
                    LET vimporte_23351_sie3 = vimporte_sie
                    IF  vimporte_23351_sie3 IS NULL THEN LET vimporte_23351_sie3 = 0
                    END IF
               END IF

               IF vsie_sie  = 4 THEN
                    LET vimporte_23351_sie4 = vimporte_sie
                    IF  vimporte_23351_sie4 IS NULL THEN LET vimporte_23351_sie4 = 0
                    END IF
               END IF

               IF vsie_sie  = 5 THEN
                    LET vimporte_23351_sie5 = vimporte_sie
                    IF  vimporte_23351_sie5 IS NULL THEN LET vimporte_23351_sie5 = 0
                    END IF
               END IF

#7 sar 92
            WHEN 28351
               IF vsie_sie  = 1 THEN
                    LET vimporte_28351_sie1 = vimporte_sie
                    IF  vimporte_28351_sie1 IS NULL THEN LET vimporte_28351_sie1 = 0
                    END IF
               END IF

               IF vsie_sie  = 2 THEN
                    LET vimporte_28351_sie2 = vimporte_sie
                    IF  vimporte_28351_sie2 IS NULL THEN LET vimporte_28351_sie2 = 0
                    END IF
               END IF

               IF vsie_sie  = 3 THEN
                    LET vimporte_28351_sie3 = vimporte_sie
                    IF  vimporte_28351_sie3 IS NULL THEN LET vimporte_28351_sie3 = 0
                    END IF
               END IF

               IF vsie_sie  = 4 THEN
                    LET vimporte_28351_sie4 = vimporte_sie
                    IF  vimporte_28351_sie4 IS NULL THEN LET vimporte_28351_sie4 = 0
                    END IF
               END IF

               IF vsie_sie  = 5 THEN
                    LET vimporte_28351_sie5 = vimporte_sie
                    IF  vimporte_28351_sie5 IS NULL THEN LET vimporte_28351_sie5 = 0
                    END IF
               END IF

#retiro issste 
            WHEN 29124
               IF vsie_sie  = 1 THEN
                    LET vimporte_29124_sie1 = vimporte_sie
                    IF  vimporte_29124_sie1 IS NULL THEN LET vimporte_29124_sie1 = 0
                    END IF
               END IF

               IF vsie_sie  = 2 THEN
                    LET vimporte_29124_sie2 = vimporte_sie
                    IF  vimporte_29124_sie2 IS NULL THEN LET vimporte_29124_sie2 = 0
                    END IF
               END IF

               IF vsie_sie  = 3 THEN
                    LET vimporte_29124_sie3 = vimporte_sie
                    IF  vimporte_29124_sie3 IS NULL THEN LET vimporte_29124_sie3 = 0
                    END IF
               END IF

               IF vsie_sie  = 4 THEN
                    LET vimporte_29124_sie4 = vimporte_sie
                    IF  vimporte_29124_sie4 IS NULL THEN LET vimporte_29124_sie4 = 0
                    END IF
               END IF

               IF vsie_sie  = 5 THEN
                    LET vimporte_29124_sie5 = vimporte_sie
                    IF  vimporte_29124_sie5 IS NULL THEN LET vimporte_29124_sie5 = 0
                    END IF
               END IF

#cv issste
            WHEN 29127
               IF vsie_sie  = 1 THEN
                    LET vimporte_29127_sie1 = vimporte_sie
                    IF  vimporte_29127_sie1 IS NULL THEN LET vimporte_29127_sie1 = 0
                    END IF
               END IF

               IF vsie_sie  = 2 THEN
                    LET vimporte_29127_sie2 = vimporte_sie
                    IF  vimporte_29127_sie2 IS NULL THEN LET vimporte_29127_sie2 = 0
                    END IF
               END IF

               IF vsie_sie  = 3 THEN
                    LET vimporte_29127_sie3 = vimporte_sie
                    IF  vimporte_29127_sie3 IS NULL THEN LET vimporte_29127_sie3 = 0
                    END IF
               END IF

               IF vsie_sie  = 4 THEN
                    LET vimporte_29127_sie4 = vimporte_sie
                    IF  vimporte_29127_sie4 IS NULL THEN LET vimporte_29127_sie4 = 0
                    END IF
               END IF

               IF vsie_sie  = 5 THEN
                    LET vimporte_29127_sie5 = vimporte_sie
                    IF  vimporte_29127_sie5 IS NULL THEN LET vimporte_29127_sie5 = 0
                    END IF
               END IF

#cs issste
            WHEN 29128
               IF vsie_sie  = 1 THEN
                    LET vimporte_29128_sie1 = vimporte_sie
                    IF  vimporte_29128_sie1 IS NULL THEN LET vimporte_29128_sie1 = 0
                    END IF
               END IF

               IF vsie_sie  = 2 THEN
                    LET vimporte_29128_sie2 = vimporte_sie
                    IF  vimporte_29128_sie2 IS NULL THEN LET vimporte_29128_sie2 = 0
                    END IF
               END IF

               IF vsie_sie  = 3 THEN
                    LET vimporte_29128_sie3 = vimporte_sie
                    IF  vimporte_29128_sie3 IS NULL THEN LET vimporte_29128_sie3 = 0
                    END IF
               END IF

               IF vsie_sie  = 4 THEN
                    LET vimporte_29128_sie4 = vimporte_sie
                    IF  vimporte_29128_sie4 IS NULL THEN LET vimporte_29128_sie4 = 0
                    END IF
               END IF

               IF vsie_sie  = 5 THEN
                    LET vimporte_29128_sie5 = vimporte_sie
                    IF  vimporte_29128_sie5 IS NULL THEN LET vimporte_29128_sie5 = 0
                    END IF
               END IF

#entradas
#1 retiro
            WHEN 21334
               IF vsie_sie  = 1 THEN
                    LET vimporte_21334_sie1 = vimporte_sie
                    IF  vimporte_21334_sie1 IS NULL THEN LET vimporte_21334_sie1 = 0
                    END IF
               END IF

               IF vsie_sie  = 2 THEN
                    LET vimporte_21334_sie2 = vimporte_sie
                    IF  vimporte_21334_sie2 IS NULL THEN LET vimporte_21334_sie2 = 0
                    END IF
               END IF

               IF vsie_sie  = 3 THEN
                    LET vimporte_21334_sie3 = vimporte_sie
                    IF  vimporte_21334_sie3 IS NULL THEN LET vimporte_21334_sie3 = 0
                    END IF
               END IF

               IF vsie_sie  = 4 THEN
                    LET vimporte_21334_sie4 = vimporte_sie
                    IF  vimporte_21334_sie4 IS NULL THEN LET vimporte_21334_sie4 = 0
                    END IF
               END IF

               IF vsie_sie  = 5 THEN
                    LET vimporte_21334_sie5 = vimporte_sie
                    IF  vimporte_21334_sie5 IS NULL THEN LET vimporte_21334_sie5 = 0
                    END IF
               END IF

#subcuenta 2,6,9 cv, estatal, especial
            WHEN 22334
               IF vsie_sie  = 1 THEN
                    LET vimporte_22334_sie1 = vimporte_sie
                    IF  vimporte_22334_sie1 IS NULL THEN LET vimporte_22334_sie1 = 0
                    END IF
               END IF

               IF vsie_sie  = 2 THEN
                    LET vimporte_22334_sie2 = vimporte_sie
                    IF  vimporte_22334_sie2 IS NULL THEN LET vimporte_22334_sie2 = 0
                    END IF
               END IF

               IF vsie_sie  = 3 THEN
                    LET vimporte_22334_sie3 = vimporte_sie
                    IF  vimporte_22334_sie3 IS NULL THEN LET vimporte_22334_sie3 = 0
                    END IF
               END IF

               IF vsie_sie  = 4 THEN
                    LET vimporte_22334_sie4 = vimporte_sie
                    IF  vimporte_22334_sie4 IS NULL THEN LET vimporte_22334_sie4 = 0
                    END IF
               END IF

               IF vsie_sie  = 5 THEN
                    LET vimporte_22334_sie5 = vimporte_sie
                    IF  vimporte_22334_sie5 IS NULL THEN LET vimporte_22334_sie5 = 0
                    END IF
               END IF

#5 cuota social
            WHEN 23334
               IF vsie_sie  = 1 THEN
                    LET vimporte_23334_sie1 = vimporte_sie
                    IF  vimporte_23334_sie1 IS NULL THEN LET vimporte_23334_sie1 = 0
                    END IF
               END IF

               IF vsie_sie  = 2 THEN
                    LET vimporte_23334_sie2 = vimporte_sie
                    IF  vimporte_23334_sie2 IS NULL THEN LET vimporte_23334_sie2 = 0
                    END IF
               END IF

               IF vsie_sie  = 3 THEN
                    LET vimporte_23334_sie3 = vimporte_sie
                    IF  vimporte_23334_sie3 IS NULL THEN LET vimporte_23334_sie3 = 0
                    END IF
               END IF

               IF vsie_sie  = 4 THEN
                    LET vimporte_23334_sie4 = vimporte_sie
                    IF  vimporte_23334_sie4 IS NULL THEN LET vimporte_23334_sie4 = 0
                    END IF
               END IF

               IF vsie_sie  = 5 THEN
                    LET vimporte_23334_sie5 = vimporte_sie
                    IF  vimporte_23334_sie5 IS NULL THEN LET vimporte_23334_sie5 = 0
                    END IF
               END IF

#7 sar 92
            WHEN 28011
               IF vsie_sie  = 1 THEN
                    LET vimporte_28011_sie1 = vimporte_sie
                    IF  vimporte_28011_sie1 IS NULL THEN LET vimporte_28011_sie1 = 0
                    END IF
               END IF

               IF vsie_sie  = 2 THEN
                    LET vimporte_28011_sie2 = vimporte_sie
                    IF  vimporte_28011_sie2 IS NULL THEN LET vimporte_28011_sie2 = 0
                    END IF
               END IF

               IF vsie_sie  = 3 THEN
                    LET vimporte_28011_sie3 = vimporte_sie
                    IF  vimporte_28011_sie3 IS NULL THEN LET vimporte_28011_sie3 = 0
                    END IF
               END IF

               IF vsie_sie  = 4 THEN
                    LET vimporte_28011_sie4 = vimporte_sie
                    IF  vimporte_28011_sie4 IS NULL THEN LET vimporte_28011_sie4 = 0
                    END IF
               END IF

               IF vsie_sie  = 5 THEN
                    LET vimporte_28011_sie5 = vimporte_sie
                    IF  vimporte_28011_sie5 IS NULL THEN LET vimporte_28011_sie5 = 0
                    END IF
               END IF

#retiro issste 
            WHEN 29024
               IF vsie_sie  = 1 THEN
                    LET vimporte_29024_sie1 = vimporte_sie
                    IF  vimporte_29024_sie1 IS NULL THEN LET vimporte_29024_sie1 = 0
                    END IF
               END IF

               IF vsie_sie  = 2 THEN
                    LET vimporte_29024_sie2 = vimporte_sie
                    IF  vimporte_29024_sie2 IS NULL THEN LET vimporte_29024_sie2 = 0
                    END IF
               END IF

               IF vsie_sie  = 3 THEN
                    LET vimporte_29024_sie3 = vimporte_sie
                    IF  vimporte_29024_sie3 IS NULL THEN LET vimporte_29024_sie3 = 0
                    END IF
               END IF

               IF vsie_sie  = 4 THEN
                    LET vimporte_29024_sie4 = vimporte_sie
                    IF  vimporte_29024_sie4 IS NULL THEN LET vimporte_29024_sie4 = 0
                    END IF
               END IF

               IF vsie_sie  = 5 THEN
                    LET vimporte_29024_sie5 = vimporte_sie
                    IF  vimporte_29024_sie5 IS NULL THEN LET vimporte_29024_sie5 = 0
                    END IF
               END IF

#cv issste
            WHEN 29027
               IF vsie_sie  = 1 THEN
                    LET vimporte_29027_sie1 = vimporte_sie
                    IF  vimporte_29027_sie1 IS NULL THEN LET vimporte_29027_sie1 = 0
                    END IF
               END IF

               IF vsie_sie  = 2 THEN
                    LET vimporte_29027_sie2 = vimporte_sie
                    IF  vimporte_29027_sie2 IS NULL THEN LET vimporte_29027_sie2 = 0
                    END IF
               END IF

               IF vsie_sie  = 3 THEN
                    LET vimporte_29027_sie3 = vimporte_sie
                    IF  vimporte_29027_sie3 IS NULL THEN LET vimporte_29027_sie3 = 0
                    END IF
               END IF

               IF vsie_sie  = 4 THEN
                    LET vimporte_29027_sie4 = vimporte_sie
                    IF  vimporte_29027_sie4 IS NULL THEN LET vimporte_29027_sie4 = 0
                    END IF
               END IF

               IF vsie_sie  = 5 THEN
                    LET vimporte_29027_sie5 = vimporte_sie
                    IF  vimporte_29027_sie5 IS NULL THEN LET vimporte_29027_sie5 = 0
                    END IF
               END IF

#cs issste
            WHEN 29028
               IF vsie_sie  = 1 THEN
                    LET vimporte_29028_sie1 = vimporte_sie
                    IF  vimporte_29028_sie1 IS NULL THEN LET vimporte_29028_sie1 = 0
                    END IF
               END IF

               IF vsie_sie  = 2 THEN
                    LET vimporte_29028_sie2 = vimporte_sie
                    IF  vimporte_29028_sie2 IS NULL THEN LET vimporte_29028_sie2 = 0
                    END IF
               END IF

               IF vsie_sie  = 3 THEN
                    LET vimporte_29028_sie3 = vimporte_sie
                    IF  vimporte_29028_sie3 IS NULL THEN LET vimporte_29028_sie3 = 0
                    END IF
               END IF

               IF vsie_sie  = 4 THEN
                    LET vimporte_29028_sie4 = vimporte_sie
                    IF  vimporte_29028_sie4 IS NULL THEN LET vimporte_29028_sie4 = 0
                    END IF
               END IF

               IF vsie_sie  = 5 THEN
                    LET vimporte_29028_sie5 = vimporte_sie
                    IF  vimporte_29028_sie5 IS NULL THEN LET vimporte_29028_sie5 = 0
                    END IF
               END IF

         END CASE
      END FOREACH
   END IF
#siefore 1 subcuenta 1
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_21315_sie1 <> 0 AND vimporte_21334_sie1 <> 0 THEN
       LET vimporte_21315_sie1_pos = vimporte_21315_sie1 * -1
#compara si la salida es mayor
         IF vimporte_21315_sie1_pos > vimporte_21334_sie1 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_21315_sie1 = vimporte_21315_sie1_pos - vimporte_21334_sie1
            LET ban21315_sie1 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_21315_sie1
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(21315,21334)
            AND   siefore         = 1

            IF vimp_1102_21315_sie1  is NULL THEN 
               LET vimp_1102_21315_sie1 = 0
            END IF

          ELSE
            LET vimporte_21315_sie1 = vimporte_21334_sie1 - vimporte_21315_sie1_pos
            LET ban21315_sie1 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_21334_sie1
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
             AND    fecha_emision = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(21315,21334)
            AND   siefore         = 1

            IF vimp_1102_21334_sie1  is NULL THEN 
               LET vimp_1102_21334_sie1 = 0
            END IF

          END IF
    ELSE
       LET ban21315_sie1 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_21315_2sie1
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(21315,21334)
       AND   siefore         = 1

       IF vimp_1102_21315_2sie1  is NULL THEN 
          LET vimp_1102_21315_2sie1 = 0
       END IF

    END IF

    IF vimp_1102_21315_2sie1 < 0 THEN
       LET vimp_1102_21315_sie1 = vimp_1102_21315_sie1 + vimp_1102_21315_2sie1
    ELSE
       LET vimp_1102_21334_sie1 = vimp_1102_21334_sie1 + vimp_1102_21315_2sie1
    END IF

#siefore 2 subcuenta 1
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_21315_sie2 <> 0 AND vimporte_21334_sie2 <> 0 THEN
       LET vimporte_21315_sie2_pos = vimporte_21315_sie2 * -1
#compara si la salida es mayor
         IF vimporte_21315_sie2_pos > vimporte_21334_sie2 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_21315_sie2 = vimporte_21315_sie2_pos - vimporte_21334_sie2
            LET ban21315_sie2 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_21315_sie2
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(21315,21334)
            AND   siefore         = 2

            IF vimp_1102_21315_sie2  is NULL THEN 
               LET vimp_1102_21315_sie2 = 0
            END IF

          ELSE
            LET vimporte_21315_sie2 = vimporte_21334_sie2 - vimporte_21315_sie2_pos
            LET ban21315_sie2 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_21334_sie2
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(21315,21334)
            AND   siefore         = 2

            IF vimp_1102_21334_sie2  is NULL THEN 
               LET vimp_1102_21334_sie2 = 0
            END IF

          END IF
    ELSE
       LET ban21315_sie2 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_21315_2sie2
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(21315,21334)
       AND   siefore         = 1

       IF vimp_1102_21315_2sie2  is NULL THEN 
          LET vimp_1102_21315_2sie2 = 0
       END IF

    END IF

    IF vimp_1102_21315_2sie2 < 1 THEN
       LET vimp_1102_21315_sie2 = vimp_1102_21315_sie2 + vimp_1102_21315_2sie2
    ELSE
       LET vimp_1102_21334_sie2 = vimp_1102_21334_sie2 + vimp_1102_21315_2sie2
    END IF

#siefore 3 subcuenta 1
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_21315_sie3 <> 0 AND vimporte_21334_sie3 <> 0 THEN
       LET vimporte_21315_sie3_pos = vimporte_21315_sie3 * -1
#compara si la salida es mayor
         IF vimporte_21315_sie3_pos > vimporte_21334_sie3 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_21315_sie3 = vimporte_21315_sie3_pos - vimporte_21334_sie3
            LET ban21315_sie3 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_21315_sie3
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(21315,21334)
            AND   siefore         = 3

            IF vimp_1102_21315_sie3  is NULL THEN 
               LET vimp_1102_21315_sie3 = 0
            END IF

          ELSE
            LET vimporte_21315_sie3 = vimporte_21334_sie3 - vimporte_21315_sie3_pos
            LET ban21315_sie3 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_21334_sie3
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(21315,21334)
            AND   siefore         = 3

            IF vimp_1102_21334_sie3  is NULL THEN 
               LET vimp_1102_21334_sie3 = 0
            END IF

          END IF
    ELSE
       LET ban21315_sie3 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_21315_2sie3
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(21315,21334)
       AND   siefore         = 1

       IF vimp_1102_21315_2sie3  is NULL THEN 
          LET vimp_1102_21315_2sie3 = 0
       END IF

    END IF

    IF vimp_1102_21315_2sie3 < 1 THEN
       LET vimp_1102_21315_sie3 = vimp_1102_21315_sie3 + vimp_1102_21315_2sie3
    ELSE
       LET vimp_1102_21334_sie3 = vimp_1102_21334_sie3 + vimp_1102_21315_2sie3
    END IF

#siefore 4 subcuenta 1
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_21315_sie4 <> 0 AND vimporte_21334_sie4 <> 0 THEN
       LET vimporte_21315_sie4_pos = vimporte_21315_sie4 * -1
#compara si la salida es mayor
         IF vimporte_21315_sie4_pos > vimporte_21334_sie4 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_21315_sie4 = vimporte_21315_sie4_pos - vimporte_21334_sie4
            LET ban21315_sie4 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_21315_sie4
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(21315,21334)
            AND   siefore         = 4

            IF vimp_1102_21315_sie4  is NULL THEN 
               LET vimp_1102_21315_sie4 = 0
            END IF

          ELSE
            LET vimporte_21315_sie4 = vimporte_21334_sie4 - vimporte_21315_sie4_pos
            LET ban21315_sie4 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_21334_sie4
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(21315,21334)
            AND   siefore         = 4

            IF vimp_1102_21334_sie4  is NULL THEN 
               LET vimp_1102_21334_sie4 = 0
            END IF

          END IF
    ELSE
       LET ban21315_sie4 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_21315_2sie4
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(21315,21334)
       AND   siefore         = 1

       IF vimp_1102_21315_2sie4  is NULL THEN 
          LET vimp_1102_21315_2sie4 = 0
       END IF

    END IF

    IF vimp_1102_21315_2sie4 < 1 THEN
       LET vimp_1102_21315_sie4 = vimp_1102_21315_sie4 + vimp_1102_21315_2sie4
    ELSE
       LET vimp_1102_21334_sie4 = vimp_1102_21334_sie4 + vimp_1102_21315_2sie4
    END IF

#siefore 5 subcuenta 1
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_21315_sie5 <> 0 AND vimporte_21334_sie5 <> 0 THEN
       LET vimporte_21315_sie5_pos = vimporte_21315_sie5 * -1
#compara si la salida es mayor
         IF vimporte_21315_sie5_pos > vimporte_21334_sie5 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_21315_sie5 = vimporte_21315_sie5_pos - vimporte_21334_sie5
            LET ban21315_sie5 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_21315_sie5
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(21315,21334)
            AND   siefore         = 5

            IF vimp_1102_21315_sie5  is NULL THEN 
               LET vimp_1102_21315_sie5 = 0
            END IF

          ELSE
            LET vimporte_21315_sie5 = vimporte_21334_sie5 - vimporte_21315_sie5_pos
            LET ban21315_sie5 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_21334_sie5
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(21315,21334)
            AND   siefore         = 5

            IF vimp_1102_21334_sie5  is NULL THEN 
               LET vimp_1102_21334_sie5 = 0
            END IF

          END IF
    ELSE
       LET ban21315_sie5 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_21315_2sie5
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(21315,21334)
       AND   siefore         = 5

       IF vimp_1102_21315_2sie5  is NULL THEN 
          LET vimp_1102_21315_2sie5 = 0
       END IF

    END IF

    IF vimp_1102_21315_2sie5 < 1 THEN
       LET vimp_1102_21315_sie5 = vimp_1102_21315_sie5 + vimp_1102_21315_2sie5
    ELSE
       LET vimp_1102_21334_sie5 = vimp_1102_21334_sie5 + vimp_1102_21315_2sie5
    END IF

#siefore 1 subcuenta 2,6,9
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_22332_sie1 <> 0 AND vimporte_22334_sie1 <> 0 THEN
       LET vimporte_22332_sie1_pos = vimporte_22332_sie1 * -1
#compara si la salida es mayor
         IF vimporte_22332_sie1_pos > vimporte_22334_sie1 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_22332_sie1 = vimporte_22332_sie1_pos - vimporte_22334_sie1
            LET ban22332_sie1 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_22332_sie1
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(22332,22334)
            AND   siefore         = 1

            IF vimp_1102_22332_sie1  is NULL THEN 
               LET vimp_1102_22332_sie1 = 0
            END IF

          ELSE
            LET vimporte_22332_sie1 = vimporte_22334_sie1 - vimporte_22332_sie1_pos
            LET ban22332_sie1 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_22334_sie1
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(22332,22334)
            AND   siefore         = 1

            IF vimp_1102_22334_sie1  is NULL THEN 
               LET vimp_1102_22334_sie1 = 0
            END IF

          END IF
    ELSE
       LET ban22332_sie1 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_22332_2sie1
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(22332,22334)
       AND   siefore         = 1

       IF vimp_1102_22332_2sie1  is NULL THEN 
          LET vimp_1102_22332_2sie1 = 0
       END IF

    END IF

    IF vimp_1102_22332_2sie1 = 1 THEN
       LET vimp_1102_22332_sie1 = vimp_1102_22332_sie1 + vimp_1102_22332_2sie1
    ELSE
       LET vimp_1102_22334_sie1 = vimp_1102_22334_sie1 + vimp_1102_22332_2sie1
    END IF

#siefore 2 subcuenta 2,6,9
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_22332_sie2 <> 0 AND vimporte_22334_sie2 <> 0 THEN
       LET vimporte_22332_sie2_pos = vimporte_22332_sie2 * -1
#compara si la salida es mayor
         IF vimporte_22332_sie2_pos > vimporte_22334_sie2 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_22332_sie2 = vimporte_22332_sie2_pos - vimporte_22334_sie2
            LET ban22332_sie2 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_22332_sie2
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(22332,22334)
            AND   siefore         = 2

            IF vimp_1102_22332_sie2  is NULL THEN 
               LET vimp_1102_22332_sie2 = 0
            END IF

          ELSE
            LET vimporte_22332_sie2 = vimporte_22334_sie2 - vimporte_22332_sie2_pos
            LET ban22332_sie2 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_22334_sie2
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(22332,22334)
            AND   siefore         = 2

            IF vimp_1102_22334_sie2  is NULL THEN 
               LET vimp_1102_22334_sie2 = 0
            END IF

          END IF
    ELSE
       LET ban22332_sie2 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_22332_2sie2
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(22332,22334)
       AND   siefore         = 2

       IF vimp_1102_22332_2sie2  is NULL THEN 
          LET vimp_1102_22332_2sie2 = 0
       END IF

    END IF

    IF vimp_1102_22332_2sie2 < 1 THEN
       LET vimp_1102_22332_sie2 = vimp_1102_22332_sie2 + vimp_1102_22332_2sie2
    ELSE
       LET vimp_1102_22334_sie2 = vimp_1102_22334_sie2 + vimp_1102_22332_2sie2
    END IF

#siefore 3 subcuenta 2,6,9
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_22332_sie3 <> 0 AND vimporte_22334_sie3 <> 0 THEN
       LET vimporte_22332_sie3_pos = vimporte_22332_sie3 * -1
#compara si la salida es mayor
         IF vimporte_22332_sie3_pos > vimporte_22334_sie3 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_22332_sie3 = vimporte_22332_sie3_pos - vimporte_22334_sie3
            LET ban22332_sie3 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_22332_sie3
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(22332,22334)
            AND   siefore         = 3

            IF vimp_1102_22332_sie3  is NULL THEN 
               LET vimp_1102_22332_sie3 = 0
            END IF

          ELSE
            LET vimporte_22332_sie3 = vimporte_22334_sie3 - vimporte_22332_sie3_pos
            LET ban22332_sie3 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_22334_sie3
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(22332,22334)
            AND   siefore         = 3

            IF vimp_1102_22334_sie3  is NULL THEN 
               LET vimp_1102_22334_sie3 = 0
            END IF

          END IF
    ELSE
       LET ban22332_sie3 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_22332_2sie3
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(22332,22334)
       AND   siefore         = 3

       IF vimp_1102_22332_2sie3  is NULL THEN 
          LET vimp_1102_22332_2sie3 = 0
       END IF

    END IF

    IF vimp_1102_22332_2sie3 < 1 THEN
       LET vimp_1102_22332_sie3 = vimp_1102_22332_sie3 + vimp_1102_22332_2sie3
    ELSE
       LET vimp_1102_22334_sie3 = vimp_1102_22334_sie3 + vimp_1102_22332_2sie3
    END IF

#siefore 4 subcuenta 2,6,9
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_22332_sie4 <> 0 AND vimporte_22334_sie4 <> 0 THEN
       LET vimporte_22332_sie4_pos = vimporte_22332_sie4 * -1
#compara si la salida es mayor
         IF vimporte_22332_sie4_pos > vimporte_22334_sie4 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_22332_sie4 = vimporte_22332_sie4_pos - vimporte_22334_sie4
            LET ban22332_sie4 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_22332_sie4
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(22332,22334)
            AND   siefore         = 4

            IF vimp_1102_22332_sie4  is NULL THEN 
               LET vimp_1102_22332_sie4 = 0
            END IF

          ELSE
            LET vimporte_22332_sie4 = vimporte_22334_sie4 - vimporte_22332_sie4_pos
            LET ban22332_sie4 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_22334_sie4
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(22332,22334)
            AND   siefore         = 4

            IF vimp_1102_22334_sie4  is NULL THEN 
               LET vimp_1102_22334_sie4 = 0
            END IF

          END IF
    ELSE
       LET ban22332_sie4 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_22332_2sie4
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(22332,22334)
       AND   siefore         = 4

       IF vimp_1102_22332_2sie4  is NULL THEN 
          LET vimp_1102_22332_2sie4 = 0
       END IF

    END IF

    IF vimp_1102_22332_2sie4 < 1 THEN
       LET vimp_1102_22332_sie4 = vimp_1102_22332_sie4 + vimp_1102_22332_2sie4
    ELSE
       LET vimp_1102_22334_sie4 = vimp_1102_22334_sie4 + vimp_1102_22332_2sie4
    END IF

#siefore 5 subcuenta 2,6,9
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_22332_sie5 <> 0 AND vimporte_22334_sie5 <> 0 THEN
       LET vimporte_22332_sie5_pos = vimporte_22332_sie5 * -1
#compara si la salida es mayor
         IF vimporte_22332_sie5_pos > vimporte_22334_sie5 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_22332_sie5 = vimporte_22332_sie5_pos - vimporte_22334_sie5
            LET ban22332_sie5 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_22332_sie5
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(22332,22334)
            AND   siefore         = 5

            IF vimp_1102_22332_sie5  is NULL THEN 
               LET vimp_1102_22332_sie5 = 0
            END IF

          ELSE
            LET vimporte_22332_sie5 = vimporte_22334_sie5 - vimporte_22332_sie5_pos
            LET ban22332_sie5 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_22334_sie5
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(22332,22334)
            AND   siefore         = 5

            IF vimp_1102_22334_sie5  is NULL THEN 
               LET vimp_1102_22334_sie5 = 0
            END IF


          END IF
    ELSE
       LET ban22332_sie5 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_22332_2sie5
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(22332,22334)
       AND   siefore         = 5

       IF vimp_1102_22332_2sie5  is NULL THEN 
          LET vimp_1102_22332_2sie5 = 0
       END IF

    END IF

    IF vimp_1102_22332_2sie5 < 1 THEN
       LET vimp_1102_22332_sie5 = vimp_1102_22332_sie5 + vimp_1102_22332_2sie5
    ELSE
       LET vimp_1102_22334_sie5 = vimp_1102_22334_sie5 + vimp_1102_22332_2sie5
    END IF

{
#siefore 5 subcuenta 1
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_21315_sie5 <> 0 AND vimporte_21334_sie5 <> 0 THEN
       LET vimporte_21315_sie5 = vimporte_21315_sie5_pos * -1
#compara si la salida es mayor
         IF vimporte_21315_sie5_pos > vimporte_21334_sie5 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_21315_sie5 = vimporte_21315_sie5_pos - vimporte_21334_sie5
            LET ban21315_sie5 = 1
          ELSE
            LET vimporte_21315_sie5 = vimporte_21334_sie5 - vimporte_21315_sie5_pos
            LET ban21315_sie5 = 2
          END IF
    ELSE
       LET ban21315_sie5 = 0
    END IF
}

#siefore 1 subcuenta 5
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_23351_sie1 <> 0 AND vimporte_23334_sie1 <> 0 THEN
       LET vimporte_23351_sie1_pos = vimporte_23351_sie1 * -1
#compara si la salida es mayor
         IF vimporte_23351_sie1_pos > vimporte_23334_sie1 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_23351_sie1 = vimporte_23351_sie1_pos - vimporte_23334_sie1
            LET ban23351_sie1 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_23351_sie1
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(23351,23334)
            AND   siefore         = 1

            IF vimp_1102_23351_sie1  is NULL THEN 
               LET vimp_1102_23351_sie1 = 0
            END IF

          ELSE
            LET vimporte_23351_sie1 = vimporte_23334_sie1 - vimporte_23351_sie1_pos
            LET ban23351_sie1 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_23334_sie1
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(23351,23334)
            AND   siefore         = 1

            IF vimp_1102_23334_sie1  is NULL THEN 
               LET vimp_1102_23334_sie1 = 0
            END IF

          END IF
    ELSE
       LET ban23351_sie1 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_23351_2sie1
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(23351,23334)
       AND   siefore         = 1

       IF vimp_1102_23351_2sie1  is NULL THEN 
          LET vimp_1102_23351_2sie1 = 0
       END IF

    END IF

    IF vimp_1102_23351_2sie1 < 1 THEN
       LET vimp_1102_23351_sie1 = vimp_1102_23351_sie1 + vimp_1102_23351_2sie1
    ELSE
       LET vimp_1102_23334_sie1 = vimp_1102_23334_sie1 + vimp_1102_23351_2sie1
    END IF

#siefore 2 subcuenta 5
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_23351_sie2 <> 0 AND vimporte_23334_sie2 <> 0 THEN
       LET vimporte_23351_sie2_pos = vimporte_23351_sie2 * -1
#compara si la salida es mayor
         IF vimporte_23351_sie2_pos > vimporte_23334_sie2 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_23351_sie2 = vimporte_23351_sie2_pos - vimporte_23334_sie2
            LET ban23351_sie2 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_23351_sie2
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(23351,23334)
            AND   siefore         = 2

            IF vimp_1102_23351_sie2  is NULL THEN 
               LET vimp_1102_23351_sie2 = 0
            END IF

          ELSE
            LET vimporte_23351_sie2 = vimporte_23334_sie2 - vimporte_23351_sie2_pos
            LET ban23351_sie2 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_23334_sie2
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(23351,23334)
            AND   siefore         = 2

            IF vimp_1102_23334_sie2  is NULL THEN 
               LET vimp_1102_23334_sie2 = 0
            END IF

          END IF
    ELSE
       LET ban23351_sie2 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_23351_2sie2
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(23351,23334)
       AND   siefore         = 2

       IF vimp_1102_23351_2sie2  is NULL THEN 
          LET vimp_1102_23351_2sie2 = 0
       END IF

    END IF

    IF vimp_1102_23351_2sie2 = 1 THEN
       LET vimp_1102_23351_sie2 = vimp_1102_23351_sie2 + vimp_1102_23351_2sie2
    ELSE
       LET vimp_1102_23334_sie2 = vimp_1102_23334_sie2 + vimp_1102_23351_2sie2
    END IF

#siefore 3 subcuenta 5
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_23351_sie3 <> 0 AND vimporte_23334_sie3 <> 0 THEN
       LET vimporte_23351_sie3_pos = vimporte_23351_sie3 * -1
#compara si la salida es mayor
         IF vimporte_23351_sie3_pos > vimporte_23334_sie3 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_23351_sie3 = vimporte_23351_sie3_pos - vimporte_23334_sie3
            LET ban23351_sie3 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_23351_sie3
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(23351,23334)
            AND   siefore         = 3

            IF vimp_1102_23351_sie3  is NULL THEN 
               LET vimp_1102_23351_sie3 = 0
            END IF

          ELSE
            LET vimporte_23351_sie3 = vimporte_23334_sie3 - vimporte_23351_sie3_pos
            LET ban23351_sie3 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_23334_sie3
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(23351,23334)
            AND   siefore         = 3

            IF vimp_1102_23334_sie3  is NULL THEN 
               LET vimp_1102_23334_sie3 = 0
            END IF

          END IF
    ELSE
       LET ban23351_sie3 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_23351_2sie3
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(23351,23334)
       AND   siefore         = 3

       IF vimp_1102_23351_2sie3  is NULL THEN 
          LET vimp_1102_23351_2sie3 = 0
       END IF

    END IF

    IF vimp_1102_23351_2sie3 < 1 THEN
       LET vimp_1102_23351_sie3 = vimp_1102_23351_sie3 + vimp_1102_23351_2sie3
    ELSE
       LET vimp_1102_23334_sie3 = vimp_1102_23334_sie3 + vimp_1102_23351_2sie3
    END IF

#siefore 4 subcuenta 5
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_23351_sie4 <> 0 AND vimporte_23334_sie4 <> 0 THEN
       LET vimporte_23351_sie4_pos = vimporte_23351_sie4 * -1
#compara si la salida es mayor
         IF vimporte_23351_sie4_pos > vimporte_23334_sie4 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_23351_sie4 = vimporte_23351_sie4_pos - vimporte_23334_sie4
            LET ban23351_sie4 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_23351_sie4
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(23351,23334)
            AND   siefore         = 4

            IF vimp_1102_23351_sie4  is NULL THEN 
               LET vimp_1102_23351_sie4 = 0
            END IF

          ELSE
            LET vimporte_23351_sie4 = vimporte_23334_sie4 - vimporte_23351_sie4_pos
            LET ban23351_sie4 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_23334_sie4
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(23351,23334)
            AND   siefore         = 4

            IF vimp_1102_23334_sie4  is NULL THEN 
               LET vimp_1102_23334_sie4 = 0
            END IF

          END IF
    ELSE
       LET ban23351_sie4 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_23351_2sie4
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(23351,23334)
       AND   siefore         = 4

       IF vimp_1102_23351_2sie4  is NULL THEN 
          LET vimp_1102_23351_2sie4 = 0
       END IF

    END IF

    IF vimp_1102_23351_2sie4 < 1 THEN
       LET vimp_1102_23351_sie4 = vimp_1102_23351_sie4 + vimp_1102_23351_2sie4
    ELSE
       LET vimp_1102_23334_sie4 = vimp_1102_23334_sie4 + vimp_1102_23351_2sie4
    END IF

#siefore 5 subcuenta 5
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_23351_sie5 <> 0 AND vimporte_23334_sie5 <> 0 THEN
       LET vimporte_23351_sie5_pos = vimporte_23351_sie5 * -1
#compara si la salida es mayor
         IF vimporte_23351_sie5_pos > vimporte_23334_sie5 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_23351_sie5 = vimporte_23351_sie5_pos - vimporte_23334_sie5
            LET ban23351_sie5 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_23351_sie5
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(23351,23334)
            AND   siefore         = 5

            IF vimp_1102_23351_sie5  is NULL THEN 
               LET vimp_1102_23351_sie5 = 0
            END IF

          ELSE
            LET vimporte_23351_sie5 = vimporte_23334_sie5 - vimporte_23351_sie5_pos
            LET ban23351_sie5 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_23334_sie5
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(23351,23334)
            AND   siefore         = 5

            IF vimp_1102_23334_sie5  is NULL THEN 
               LET vimp_1102_23334_sie5 = 0
            END IF

          END IF
    ELSE
       LET ban23351_sie5 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_23351_2sie5
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(23351,23334)
       AND   siefore         = 5

       IF vimp_1102_23351_2sie5  is NULL THEN 
          LET vimp_1102_23351_2sie5 = 0
       END IF

    END IF

    IF vimp_1102_23351_2sie5 < 1 THEN
       LET vimp_1102_23351_sie5 = vimp_1102_23351_sie5 + vimp_1102_23351_2sie5
    ELSE
       LET vimp_1102_23334_sie5 = vimp_1102_23334_sie5 + vimp_1102_23351_2sie5
    END IF

#siefore 1 subcuenta 7
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_28351_sie1 <> 0 AND vimporte_28011_sie1 <> 0 THEN
       LET vimporte_28351_sie1_pos = vimporte_28351_sie1 * -1
#compara si la salida es mayor
         IF vimporte_28351_sie1_pos > vimporte_28011_sie1 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_28351_sie1 = vimporte_28351_sie1_pos - vimporte_28011_sie1
            LET ban28351_sie1 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_28351_sie1
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(28351,28011)
            AND   siefore         = 1

            IF vimp_1102_28351_sie1  is NULL THEN 
               LET vimp_1102_28351_sie1 = 0
            END IF

          ELSE
            LET vimporte_28351_sie1 = vimporte_28011_sie1 - vimporte_28351_sie1_pos
            LET ban28351_sie1 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_28011_sie1
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(28351,28011)
            AND   siefore         = 1

            IF vimp_1102_28011_sie1  is NULL THEN 
               LET vimp_1102_28011_sie1 = 0
            END IF

          END IF
    ELSE
       LET ban28351_sie1 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_28351_2sie1
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(28351,28011)
       AND   siefore         = 1

       IF vimp_1102_28351_2sie1  is NULL THEN 
          LET vimp_1102_28351_2sie1 = 0
       END IF

    END IF

    IF vimp_1102_28351_2sie1 < 1 THEN
       LET vimp_1102_28351_sie1 = vimp_1102_28351_sie1 + vimp_1102_28351_2sie1
    ELSE
       LET vimp_1102_28011_sie1 = vimp_1102_28011_sie1 + vimp_1102_28351_2sie1
    END IF

#siefore 2 subcuenta 7
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_28351_sie2 <> 0 AND vimporte_28011_sie2 <> 0 THEN
       LET vimporte_28351_sie2_pos = vimporte_28351_sie2 * -1
#compara si la salida es mayor
         IF vimporte_28351_sie2_pos > vimporte_28011_sie2 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_28351_sie2 = vimporte_28351_sie2_pos - vimporte_28011_sie2
            LET ban28351_sie2 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_28351_sie2
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(28351,28011)
            AND   siefore         = 2

            IF vimp_1102_28351_sie2  is NULL THEN 
               LET vimp_1102_28351_sie2 = 0
            END IF

          ELSE
            LET vimporte_28351_sie2 = vimporte_28011_sie2 - vimporte_28351_sie2_pos
            LET ban28351_sie2 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_28011_sie2
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(28351,28011)
            AND   siefore         = 2

            IF vimp_1102_28011_sie2  is NULL THEN 
               LET vimp_1102_28011_sie2 = 0
            END IF

          END IF
    ELSE
       LET ban28351_sie2 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_28351_2sie2
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(28351,28011)
       AND   siefore         = 2

       IF vimp_1102_28351_2sie2  is NULL THEN 
          LET vimp_1102_28351_2sie2 = 0
       END IF

    END IF

    IF vimp_1102_28351_2sie2 < 1 THEN
       LET vimp_1102_28351_sie2 = vimp_1102_28351_sie2 + vimp_1102_28351_2sie2
    ELSE
       LET vimp_1102_28011_sie2 = vimp_1102_28011_sie2 + vimp_1102_28351_2sie2
    END IF

#siefore 3 subcuenta 7
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_28351_sie3 <> 0 AND vimporte_28011_sie3 <> 0 THEN
       LET vimporte_28351_sie3_pos = vimporte_28351_sie3 * -1
#compara si la salida es mayor
         IF vimporte_28351_sie3_pos > vimporte_28011_sie3 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_28351_sie3 = vimporte_28351_sie3_pos - vimporte_28011_sie3
            LET ban28351_sie3 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_28351_sie3
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(28351,28011)
            AND   siefore         = 3

            IF vimp_1102_28351_sie3  is NULL THEN 
               LET vimp_1102_28351_sie3 = 0
            END IF

          ELSE
            LET vimporte_28351_sie3 = vimporte_28011_sie3 - vimporte_28351_sie3_pos
            LET ban28351_sie3 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_28011_sie3
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(28351,28011)
            AND   siefore         = 3

            IF vimp_1102_28011_sie3  is NULL THEN 
               LET vimp_1102_28011_sie3 = 0
            END IF

          END IF
    ELSE
       LET ban28351_sie3 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_28351_2sie3
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(28351,28011)
       AND   siefore         = 3

       IF vimp_1102_28351_2sie3  is NULL THEN 
          LET vimp_1102_28351_2sie3 = 0
       END IF

    END IF

    IF vimp_1102_28351_2sie3 < 1 THEN
       LET vimp_1102_28351_sie3 = vimp_1102_28351_sie3 + vimp_1102_28351_2sie3
    ELSE
       LET vimp_1102_28011_sie3 = vimp_1102_28011_sie3 + vimp_1102_28351_2sie3
    END IF

#siefore 4 subcuenta 7
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_28351_sie4 <> 0 AND vimporte_28011_sie4 <> 0 THEN
       LET vimporte_28351_sie4_pos = vimporte_28351_sie4 * -1
#compara si la salida es mayor
         IF vimporte_28351_sie4_pos > vimporte_28011_sie4 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_28351_sie4 = vimporte_28351_sie4_pos - vimporte_28011_sie4
            LET ban28351_sie4 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_28351_sie4
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(28351,28011)
            AND   siefore         = 4

            IF vimp_1102_28351_sie4  is NULL THEN 
               LET vimp_1102_28351_sie4 = 0
            END IF

          ELSE
            LET vimporte_28351_sie4 = vimporte_28011_sie4 - vimporte_28351_sie4_pos
            LET ban28351_sie4 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_28011_sie4
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(28351,28011)
            AND   siefore         = 4

            IF vimp_1102_28011_sie4  is NULL THEN 
               LET vimp_1102_28011_sie4 = 0
            END IF

          END IF
    ELSE
       LET ban28351_sie4 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_28351_2sie4
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(28351,28011)
       AND   siefore         = 4

       IF vimp_1102_28351_2sie4  is NULL THEN 
          LET vimp_1102_28351_2sie4 = 0
       END IF

    END IF

    IF vimp_1102_28351_2sie4 < 1 THEN
       LET vimp_1102_28351_sie4 = vimp_1102_28351_sie4 + vimp_1102_28351_2sie4
    ELSE
       LET vimp_1102_28011_sie4 = vimp_1102_28011_sie4 + vimp_1102_28351_2sie4
    END IF

#siefore 5 subcuenta 7
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_28351_sie5 <> 0 AND vimporte_28011_sie5 <> 0 THEN
       LET vimporte_28351_sie5_pos = vimporte_28351_sie5 * -1
#compara si la salida es mayor
         IF vimporte_28351_sie5_pos > vimporte_28011_sie5 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_28351_sie5 = vimporte_28351_sie5_pos - vimporte_28011_sie5
            LET ban28351_sie5 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_28351_sie5
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(28351,28011)
            AND   siefore         = 5

            IF vimp_1102_28351_sie5  is NULL THEN 
               LET vimp_1102_28351_sie5 = 0
            END IF

          ELSE
            LET vimporte_28351_sie5 = vimporte_28011_sie5 - vimporte_28351_sie5_pos
            LET ban28351_sie5 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_28011_sie5
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(28351,28011)
            AND   siefore         = 5

            IF vimp_1102_28011_sie5  is NULL THEN 
               LET vimp_1102_28011_sie5 = 0
            END IF

          END IF
    ELSE
       LET ban28351_sie5 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_28351_2sie5
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(28351,28011)
       AND   siefore         = 5

       IF vimp_1102_28351_2sie5  is NULL THEN 
          LET vimp_1102_28351_2sie5 = 0
       END IF

    END IF

    IF vimp_1102_28351_2sie5 < 1 THEN
       LET vimp_1102_28351_sie5 = vimp_1102_28351_sie5 + vimp_1102_28351_2sie5
    ELSE
       LET vimp_1102_28011_sie5 = vimp_1102_28011_sie5 + vimp_1102_28351_2sie5
    END IF

#>>>>>>>>
#siefore 1 subcuenta 30
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_29124_sie1 <> 0 AND vimporte_29024_sie1 <> 0 THEN
       LET vimporte_29124_sie1_pos = vimporte_29124_sie1 * -1
#compara si la salida es mayor
         IF vimporte_29124_sie1_pos > vimporte_29024_sie1 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_29124_sie1 = vimporte_29124_sie1_pos - vimporte_29024_sie1
            LET ban29124_sie1 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_29124_sie1
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29124,29024)
            AND   siefore         = 1

            IF vimp_1102_29124_sie1  is NULL THEN 
               LET vimp_1102_29124_sie1 = 0
            END IF

          ELSE
            LET vimporte_29124_sie1 = vimporte_29024_sie1 - vimporte_29124_sie1_pos
            LET ban29124_sie1 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_29024_sie1
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29124,29024)
            AND   siefore         = 1

            IF vimp_1102_29024_sie1  is NULL THEN 
               LET vimp_1102_29024_sie1 = 0
            END IF

          END IF
    ELSE
       LET ban29124_sie1 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_29124_2sie1
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(29124,29024)
       AND   siefore         = 1

       IF vimp_1102_29124_2sie1  is NULL THEN 
          LET vimp_1102_29124_2sie1 = 0
       END IF

    END IF

    IF vimp_1102_29124_2sie1 < 1 THEN
       LET vimp_1102_29124_sie1 = vimp_1102_29124_sie1 + vimp_1102_29124_2sie1
    ELSE
       LET vimp_1102_29024_sie1 = vimp_1102_29024_sie1 + vimp_1102_29124_2sie1
    END IF

#siefore 2 subcuenta 30
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_29124_sie2 <> 0 AND vimporte_29024_sie2 <> 0 THEN
       LET vimporte_29124_sie2_pos = vimporte_29124_sie2 * -1
#compara si la salida es mayor
         IF vimporte_29124_sie2_pos > vimporte_29024_sie2 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_29124_sie2 = vimporte_29124_sie2_pos - vimporte_29024_sie2
            LET ban29124_sie2 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_29124_sie2
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29124,29024)
            AND   siefore         = 2

            IF vimp_1102_29124_sie2  is NULL THEN 
               LET vimp_1102_29124_sie2 = 0
            END IF

          ELSE
            LET vimporte_29124_sie2 = vimporte_29024_sie2 - vimporte_29124_sie2_pos
            LET ban29124_sie2 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_29024_sie2
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29124,29024)
            AND   siefore         = 2

            IF vimp_1102_29024_sie2  is NULL THEN 
               LET vimp_1102_29024_sie2 = 0
            END IF

          END IF
    ELSE
       LET ban29124_sie2 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_29124_2sie2
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(29124,29024)
       AND   siefore         = 2

       IF vimp_1102_29124_2sie2  is NULL THEN 
          LET vimp_1102_29124_2sie2 = 0
       END IF

    END IF

    IF vimp_1102_29124_2sie2 = 1 THEN
       LET vimp_1102_29124_sie2 = vimp_1102_29124_sie2 + vimp_1102_29124_2sie2
    ELSE
       LET vimp_1102_29024_sie2 = vimp_1102_29024_sie2 + vimp_1102_29124_2sie2
    END IF

#siefore 3 subcuenta 30
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_29124_sie3 <> 0 AND vimporte_29024_sie3 <> 0 THEN
       LET vimporte_29124_sie3_pos = vimporte_29124_sie3 * -1
#compara si la salida es mayor
         IF vimporte_29124_sie3_pos > vimporte_29024_sie3 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_29124_sie3 = vimporte_29124_sie3_pos - vimporte_29024_sie3
            LET ban29124_sie3 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_29124_sie3
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29124,29024)
            AND   siefore         = 3

            IF vimp_1102_29124_sie3  is NULL THEN 
               LET vimp_1102_29124_sie3 = 0
            END IF

          ELSE
            LET vimporte_29124_sie3 = vimporte_29024_sie3 - vimporte_29124_sie3_pos
            LET ban29124_sie3 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_29024_sie3
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29124,29024)
            AND   siefore         = 3

            IF vimp_1102_29024_sie3  is NULL THEN 
               LET vimp_1102_29024_sie3 = 0
            END IF

          END IF
    ELSE
       LET ban29124_sie3 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_29124_2sie3
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(29124,29024)
       AND   siefore         = 3

       IF vimp_1102_29124_2sie3  is NULL THEN 
          LET vimp_1102_29124_2sie3 = 0
       END IF

    END IF

    IF vimp_1102_29124_2sie3 < 1 THEN
       LET vimp_1102_29124_sie3 = vimp_1102_29124_sie3 + vimp_1102_29124_2sie3
    ELSE
       LET vimp_1102_29024_sie3 = vimp_1102_29024_sie3 + vimp_1102_29124_2sie3
    END IF

#siefore 4 subcuenta 30
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_29124_sie4 <> 0 AND vimporte_29024_sie4 <> 0 THEN
       LET vimporte_29124_sie4_pos = vimporte_29124_sie4 * -1
#compara si la salida es mayor
         IF vimporte_29124_sie4_pos > vimporte_29024_sie4 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_29124_sie4 = vimporte_29124_sie4_pos - vimporte_29024_sie4
            LET ban29124_sie4 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_29124_sie4
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29124,29024)
            AND   siefore         = 4

            IF vimp_1102_29124_sie4  is NULL THEN 
               LET vimp_1102_29124_sie4 = 0
            END IF

          ELSE
            LET vimporte_29124_sie4 = vimporte_29024_sie4 - vimporte_29124_sie4_pos
            LET ban29124_sie4 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_29024_sie4
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29124,29024)
            AND   siefore         = 4

            IF vimp_1102_29024_sie4  is NULL THEN 
               LET vimp_1102_29024_sie4 = 0
            END IF

          END IF
    ELSE
       LET ban29124_sie4 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_29124_2sie4
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(29124,29024)
       AND   siefore         = 4

       IF vimp_1102_29124_2sie4  is NULL THEN 
          LET vimp_1102_29124_2sie4 = 0
       END IF

    END IF

    IF vimp_1102_29124_2sie4 < 1 THEN
       LET vimp_1102_29124_sie4 = vimp_1102_29124_sie4 + vimp_1102_29124_2sie4
    ELSE
       LET vimp_1102_29024_sie4 = vimp_1102_29024_sie4 + vimp_1102_29124_2sie4
    END IF

#siefore 5 subcuenta 30
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_29124_sie5 <> 0 AND vimporte_29024_sie5 <> 0 THEN
       LET vimporte_29124_sie5_pos = vimporte_29124_sie5 * -1
#compara si la salida es mayor
         IF vimporte_29124_sie5_pos > vimporte_29024_sie5 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_29124_sie5 = vimporte_29124_sie5_pos - vimporte_29024_sie5
            LET ban29124_sie5 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_29124_sie5
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29124,29024)
            AND   siefore         = 5

            IF vimp_1102_29124_sie5  is NULL THEN 
               LET vimp_1102_29124_sie5 = 0
            END IF

          ELSE
            LET vimporte_29124_sie5 = vimporte_29024_sie5 - vimporte_29124_sie5_pos
            LET ban29124_sie5 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_29024_sie5
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29124,29024)
            AND   siefore         = 5

            IF vimp_1102_29024_sie5  is NULL THEN 
               LET vimp_1102_29024_sie5 = 0
            END IF

          END IF
    ELSE
       LET ban29124_sie5 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_29124_2sie5
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(29124,29024)
       AND   siefore         = 5

       IF vimp_1102_29124_2sie5  is NULL THEN 
          LET vimp_1102_29124_2sie5 = 0
       END IF

    END IF

    IF vimp_1102_29124_2sie5 < 1 THEN
       LET vimp_1102_29124_sie5 = vimp_1102_29124_sie5 + vimp_1102_29124_2sie5
    ELSE
       LET vimp_1102_29024_sie5 = vimp_1102_29024_sie5 + vimp_1102_29124_2sie5
    END IF

#<<<<<<<<
##>>>>>>>>>>
#siefore 1 subcuenta 31
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_29127_sie1 <> 0 AND vimporte_29027_sie1 <> 0 THEN
       LET vimporte_29127_sie1_pos = vimporte_29127_sie1 * -1
#compara si la salida es mayor
         IF vimporte_29127_sie1_pos > vimporte_29027_sie1 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_29127_sie1 = vimporte_29127_sie1_pos - vimporte_29027_sie1
            LET ban29127_sie1 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_29127_sie1
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29127,29027)
            AND   siefore         = 1

            IF vimp_1102_29127_sie1  is NULL THEN 
               LET vimp_1102_29127_sie1 = 0
            END IF

          ELSE
            LET vimporte_29127_sie1 = vimporte_29027_sie1 - vimporte_29127_sie1_pos
            LET ban29127_sie1 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_29027_sie1
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29127,29027)
            AND   siefore         = 1

            IF vimp_1102_29027_sie1  is NULL THEN 
               LET vimp_1102_29027_sie1 = 0
            END IF

          END IF
    ELSE
       LET ban29127_sie1 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_29127_2sie1
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(29127,29027)
       AND   siefore         = 1

       IF vimp_1102_29127_2sie1  is NULL THEN 
          LET vimp_1102_29127_2sie1 = 0
       END IF

    END IF

    IF vimp_1102_29127_2sie1 < 1 THEN
       LET vimp_1102_29127_sie1 = vimp_1102_29127_sie1 + vimp_1102_29127_2sie1
    ELSE
       LET vimp_1102_29027_sie1 = vimp_1102_29027_sie1 + vimp_1102_29127_2sie1
    END IF

#siefore 2 subcuenta 31
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_29127_sie2 <> 0 AND vimporte_29027_sie2 <> 0 THEN
       LET vimporte_29127_sie2_pos = vimporte_29127_sie2 * -1
#compara si la salida es mayor
         IF vimporte_29127_sie2_pos > vimporte_29027_sie2 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_29127_sie2 = vimporte_29127_sie2_pos - vimporte_29027_sie2
            LET ban29127_sie2 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_29127_sie2
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29127,29027)
            AND   siefore         = 2

            IF vimp_1102_29127_sie2  is NULL THEN 
               LET vimp_1102_29127_sie2 = 0
            END IF

          ELSE
            LET vimporte_29127_sie2 = vimporte_29027_sie2 - vimporte_29127_sie2_pos
            LET ban29127_sie2 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_29027_sie2
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29127,29027)
            AND   siefore         = 2

            IF vimp_1102_29027_sie2  is NULL THEN 
               LET vimp_1102_29027_sie2 = 0
            END IF

          END IF
    ELSE
       LET ban29127_sie2 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_29127_2sie2
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(29127,29027)
       AND   siefore         = 2

       IF vimp_1102_29127_2sie2  is NULL THEN 
          LET vimp_1102_29127_2sie2 = 0
       END IF

    END IF

    IF vimp_1102_29127_2sie2 < 1 THEN
       LET vimp_1102_29127_sie2 = vimp_1102_29127_sie2 + vimp_1102_29127_2sie2
    ELSE
       LET vimp_1102_29027_sie2 = vimp_1102_29027_sie2 + vimp_1102_29127_2sie2
    END IF

#siefore 3 subcuenta 31
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_29127_sie3 <> 0 AND vimporte_29027_sie3 <> 0 THEN
       LET vimporte_29127_sie3_pos = vimporte_29127_sie3 * -1
#compara si la salida es mayor
         IF vimporte_29127_sie3_pos > vimporte_29027_sie3 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_29127_sie3 = vimporte_29127_sie3_pos - vimporte_29027_sie3
            LET ban29127_sie3 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_29127_sie3
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29127,29027)
            AND   siefore         = 3

            IF vimp_1102_29127_sie3  is NULL THEN 
               LET vimp_1102_29127_sie3 = 0
            END IF

          ELSE
            LET vimporte_29127_sie3 = vimporte_29027_sie3 - vimporte_29127_sie3_pos
            LET ban29127_sie3 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_29027_sie3
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29127,29027)
            AND   siefore         = 3

            IF vimp_1102_29027_sie3  is NULL THEN 
               LET vimp_1102_29027_sie3 = 0
            END IF

          END IF
    ELSE
       LET ban29127_sie3 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_29127_2sie3
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(29127,29027)
       AND   siefore         = 3

       IF vimp_1102_29127_2sie3  is NULL THEN 
          LET vimp_1102_29127_2sie3 = 0
       END IF

    END IF

    IF vimp_1102_29127_2sie3 < 1 THEN
       LET vimp_1102_29127_sie3 = vimp_1102_29127_sie3 + vimp_1102_29127_2sie3
    ELSE
       LET vimp_1102_29027_sie3 = vimp_1102_29027_sie3 + vimp_1102_29127_2sie3
    END IF

#siefore 4 subcuenta 31
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_29127_sie4 <> 0 AND vimporte_29027_sie4 <> 0 THEN
       LET vimporte_29127_sie4_pos = vimporte_29127_sie4 * -1
#compara si la salida es mayor
         IF vimporte_29127_sie4_pos > vimporte_29027_sie4 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_29127_sie4 = vimporte_29127_sie4_pos - vimporte_29027_sie4
            LET ban29127_sie4 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_29127_sie4
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29127,29027)
            AND   siefore         = 4

            IF vimp_1102_29127_sie4  is NULL THEN 
               LET vimp_1102_29127_sie4 = 0
            END IF

          ELSE
            LET vimporte_29127_sie4 = vimporte_29027_sie4 - vimporte_29127_sie4_pos
            LET ban29127_sie4 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_29027_sie4
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29127,29027)
            AND   siefore         = 4

            IF vimp_1102_29027_sie4  is NULL THEN 
               LET vimp_1102_29027_sie4 = 0
            END IF

          END IF
    ELSE
       LET ban29127_sie4 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_29127_2sie4
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(29127,29027)
       AND   siefore         = 4

       IF vimp_1102_29127_2sie4  is NULL THEN 
          LET vimp_1102_29127_2sie4 = 0
       END IF

    END IF

    IF vimp_1102_29127_2sie4 = 1 THEN
       LET vimp_1102_29127_sie4 = vimp_1102_29127_sie4 + vimp_1102_29127_2sie4
    ELSE
       LET vimp_1102_29027_sie4 = vimp_1102_29027_sie4 + vimp_1102_29127_2sie4
    END IF

#siefore 5 subcuenta 31
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_29127_sie5 <> 0 AND vimporte_29027_sie5 <> 0 THEN
       LET vimporte_29127_sie5_pos = vimporte_29127_sie5 * -1
#compara si la salida es mayor
         IF vimporte_29127_sie5_pos > vimporte_29027_sie5 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_29127_sie5 = vimporte_29127_sie5_pos - vimporte_29027_sie5
            LET ban29127_sie5 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_29127_sie5
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29127,29027)
            AND   siefore         = 5

            IF vimp_1102_29127_sie5  is NULL THEN 
               LET vimp_1102_29127_sie5 = 0
            END IF

          ELSE
            LET vimporte_29127_sie5 = vimporte_29027_sie5 - vimporte_29127_sie5_pos
            LET ban29127_sie5 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_29027_sie5
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29127,29027)
            AND   siefore         = 5

            IF vimp_1102_29027_sie5  is NULL THEN 
               LET vimp_1102_29027_sie5 = 0
            END IF

          END IF
    ELSE
       LET ban29127_sie5 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_29127_2sie5
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(29127,29027)
       AND   siefore         = 5

       IF vimp_1102_29127_2sie5  is NULL THEN 
          LET vimp_1102_29127_2sie5 = 0
       END IF

    END IF

    IF vimp_1102_29127_2sie5 < 1 THEN
       LET vimp_1102_29127_sie5 = vimp_1102_29127_sie5 + vimp_1102_29127_2sie5
    ELSE
       LET vimp_1102_29027_sie5 = vimp_1102_29027_sie5 + vimp_1102_29127_2sie5
    END IF

##<<<<<<<<<<

###>>>>>>>>
#siefore 1 subcuenta 32
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_29128_sie1 <> 0 AND vimporte_29028_sie1 <> 0 THEN
       LET vimporte_29128_sie1_pos = vimporte_29128_sie1 * -1
#compara si la salida es mayor
         IF vimporte_29128_sie1_pos > vimporte_29028_sie1 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_29128_sie1 = vimporte_29128_sie1_pos - vimporte_29028_sie1
            LET ban29128_sie1 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_29128_sie1
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29128,29028)
            AND   siefore         = 1

            IF vimp_1102_29128_sie1  is NULL THEN 
               LET vimp_1102_29128_sie1 = 0
            END IF

          ELSE
            LET vimporte_29128_sie1 = vimporte_29028_sie1 - vimporte_29128_sie1_pos
            LET ban29128_sie1 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_29028_sie1
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29128,29028)
            AND   siefore         = 1

            IF vimp_1102_29028_sie1  is NULL THEN 
               LET vimp_1102_29028_sie1 = 0
            END IF

          END IF
    ELSE
       LET ban29128_sie1 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_29128_2sie1
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(29128,29028)
       AND   siefore         = 1

       IF vimp_1102_29128_2sie1  is NULL THEN 
          LET vimp_1102_29128_2sie1 = 0
       END IF

    END IF

    IF vimp_1102_29128_2sie1 < 1 THEN
       LET vimp_1102_29128_sie1 = vimp_1102_29128_sie1 + vimp_1102_29128_2sie1
    ELSE
       LET vimp_1102_29028_sie1 = vimp_1102_29028_sie1 + vimp_1102_29128_2sie1
    END IF

#siefore 2 subcuenta 32
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_29128_sie2 <> 0 AND vimporte_29028_sie2 <> 0 THEN
       LET vimporte_29128_sie2_pos = vimporte_29128_sie2 * -1
#compara si la salida es mayor
         IF vimporte_29128_sie2_pos > vimporte_29028_sie2 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_29128_sie2 = vimporte_29128_sie2_pos - vimporte_29028_sie2
            LET ban29128_sie2 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_29128_sie2
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29128,29028)
            AND   siefore         = 2

            IF vimp_1102_29128_sie2  is NULL THEN 
               LET vimp_1102_29128_sie2 = 0
            END IF

          ELSE
            LET vimporte_29128_sie2 = vimporte_29028_sie2 - vimporte_29128_sie2_pos
            LET ban29128_sie2 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_29028_sie2
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29128,29028)
            AND   siefore         = 2

            IF vimp_1102_29028_sie2  is NULL THEN 
               LET vimp_1102_29028_sie2 = 0
            END IF

          END IF
    ELSE
       LET ban29128_sie2 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_29128_2sie2
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(29128,29028)
       AND   siefore         = 2

       IF vimp_1102_29128_2sie2  is NULL THEN 
          LET vimp_1102_29128_2sie2 = 0
       END IF

    END IF

    IF vimp_1102_29128_2sie2 < 1 THEN
       LET vimp_1102_29128_sie2 = vimp_1102_29128_sie2 + vimp_1102_29128_2sie2
    ELSE
       LET vimp_1102_29028_sie2 = vimp_1102_29028_sie2 + vimp_1102_29128_2sie2
    END IF

#siefore 3 subcuenta 32
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_29128_sie3 <> 0 AND vimporte_29028_sie3 <> 0 THEN
       LET vimporte_29128_sie3_pos = vimporte_29128_sie3 * -1
#compara si la salida es mayor
         IF vimporte_29128_sie3_pos > vimporte_29028_sie3 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_29128_sie3 = vimporte_29128_sie3_pos - vimporte_29028_sie3
            LET ban29128_sie3 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_29128_sie3
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29128,29028)
            AND   siefore         = 3

            IF vimp_1102_29128_sie3  is NULL THEN 
               LET vimp_1102_29128_sie3 = 0
            END IF

          ELSE
            LET vimporte_29128_sie3 = vimporte_29028_sie3 - vimporte_29128_sie3_pos
            LET ban29128_sie3 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_29028_sie3
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29128,29028)
            AND   siefore         = 3

            IF vimp_1102_29028_sie3  is NULL THEN 
               LET vimp_1102_29028_sie3 = 0
            END IF

          END IF
    ELSE
       LET ban29128_sie3 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_29128_2sie3
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(29128,29028)
       AND   siefore         = 3

       IF vimp_1102_29128_2sie3  is NULL THEN 
          LET vimp_1102_29128_2sie3 = 0
       END IF

    END IF

    IF vimp_1102_29128_2sie3 < 1 THEN
       LET vimp_1102_29128_sie3 = vimp_1102_29128_sie3 + vimp_1102_29128_2sie3
    ELSE 
       LET vimp_1102_29028_sie3 = vimp_1102_29028_sie3 + vimp_1102_29128_2sie3
    END IF

#siefore 4 subcuenta 32
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_29128_sie4 <> 0 AND vimporte_29028_sie4 <> 0 THEN
       LET vimporte_29128_sie4_pos = vimporte_29128_sie4 * -1
#compara si la salida es mayor
         IF vimporte_29128_sie4_pos > vimporte_29028_sie4 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_29128_sie4 = vimporte_29128_sie4_pos - vimporte_29028_sie4
            LET ban29128_sie4 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_29128_sie4
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29128,29028)
            AND   siefore         = 4

            IF vimp_1102_29128_sie4  is NULL THEN 
               LET vimp_1102_29128_sie4 = 0
            END IF

          ELSE
            LET vimporte_29128_sie4 = vimporte_29028_sie4 - vimporte_29128_sie4_pos
            LET ban29128_sie4 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_29028_sie4
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29128,29028)
            AND   siefore         = 4

            IF vimp_1102_29028_sie4  is NULL THEN 
               LET vimp_1102_29028_sie4 = 0
            END IF

          END IF
    ELSE
       LET ban29128_sie4 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_29128_2sie4
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(29128,29028)
       AND   siefore         = 4

       IF vimp_1102_29128_2sie4  is NULL THEN 
          LET vimp_1102_29128_2sie4 = 0
       END IF

    END IF

    IF vimp_1102_29128_2sie4 < 1 THEN
       LET vimp_1102_29128_sie4 = vimp_1102_29128_sie4 + vimp_1102_29128_2sie4
    ELSE
       LET vimp_1102_29028_sie4 = vimp_1102_29028_sie4 + vimp_1102_29128_2sie4
    END IF

#siefore 5 subcuenta 32
#verifica si hay salidas y entradas al mismo tiempo
    IF vimporte_29128_sie5 <> 0 AND vimporte_29028_sie5 <> 0 THEN
       LET vimporte_29128_sie5_pos = vimporte_29128_sie5 * -1
#compara si la salida es mayor
         IF vimporte_29128_sie5_pos > vimporte_29028_sie5 THEN
#si la salida es mayor, netea y prende bandera 1 = 1
            LET vimporte_29128_sie5 = vimporte_29128_sie5_pos - vimporte_29028_sie5
            LET ban29128_sie5 = 1

            SELECT SUM(importe)
            INTO  vimp_1102_29128_sie5
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29128,29028)
            AND   siefore         = 5

            IF vimp_1102_29128_sie5  is NULL THEN 
               LET vimp_1102_29128_sie5 = 0
            END IF

          ELSE
            LET vimporte_29128_sie5 = vimporte_29028_sie5 - vimporte_29128_sie5_pos
            LET ban29128_sie5 = 2

            SELECT SUM(importe)
            INTO  vimp_1102_29028_sie5
            FROM  con_transaccion
            WHERE proceso_cod     = '00039'
            --AND   folio           = 14726
            AND    fecha_emision  = vfecha_reporte
            AND   identificador   = 1
            AND   transaccion_cod IN(29128,29028)
            AND   siefore         = 5

            IF vimp_1102_29028_sie5  is NULL THEN 
               LET vimp_1102_29028_sie5 = 0
            END IF

          END IF
    ELSE
       LET ban29128_sie5 = 0

       SELECT SUM(importe)
       INTO  vimp_1102_29128_2sie5
       FROM  con_transaccion
       WHERE proceso_cod     = '00039'
       --AND   folio           = 14726
       AND    fecha_emision = vfecha_reporte
       AND   identificador   = 1
       AND   transaccion_cod IN(29128,29028)
       AND   siefore         = 5

       IF vimp_1102_29128_2sie5  is NULL THEN 
          LET vimp_1102_29128_2sie5 = 0
       END IF

    END IF

    IF vimp_1102_29128_2sie5 < 1 THEN
       LET vimp_1102_29128_sie5 = vimp_1102_29128_sie5 + vimp_1102_29128_2sie5
    ELSE
       LET vimp_1102_29028_sie5 = vimp_1102_29028_sie5 + vimp_1102_29128_2sie5
    END IF

###>>>>>>>>

    LET imp_1101_ent = 
                    vimp_1102_21315_sie1 +
                    vimp_1102_21315_sie2 +
                    vimp_1102_21315_sie3 +
                    vimp_1102_21315_sie4 +
                    vimp_1102_21315_sie5 +
                    vimp_1102_22332_sie1 +
                    vimp_1102_22332_sie2 +
                    vimp_1102_22332_sie3 +
                    vimp_1102_22332_sie4 +
                    vimp_1102_22332_sie5 +
                    vimp_1102_23351_sie1 +
                    vimp_1102_23351_sie2 +
                    vimp_1102_23351_sie3 +
                    vimp_1102_23351_sie4 +
                    vimp_1102_23351_sie5 +
                    vimp_1102_28351_sie1 +
                    vimp_1102_28351_sie2 +
                    vimp_1102_28351_sie3 +
                    vimp_1102_28351_sie4 +
                    vimp_1102_28351_sie5 +
                    vimp_1102_29124_sie1 +
                    vimp_1102_29124_sie2 +
                    vimp_1102_29124_sie3 +
                    vimp_1102_29124_sie4 +
                    vimp_1102_29124_sie5 +
                    vimp_1102_29127_sie1 +
                    vimp_1102_29127_sie2 +
                    vimp_1102_29127_sie3 +
                    vimp_1102_29127_sie4 +
                    vimp_1102_29127_sie5 +
                    vimp_1102_29128_sie1 +
                    vimp_1102_29128_sie2 +
                    vimp_1102_29128_sie3 +
                    vimp_1102_29128_sie4 +
                    vimp_1102_29128_sie5 

         LET imp_1101_sal =
                    vimp_1102_21334_sie1 +
                    vimp_1102_21334_sie2 +
                    vimp_1102_21334_sie3 +
                    vimp_1102_21334_sie4 +
                    vimp_1102_21334_sie5 +
                    vimp_1102_22334_sie1 +
                    vimp_1102_22334_sie2 +
                    vimp_1102_22334_sie3 +
                    vimp_1102_22334_sie4 +
                    vimp_1102_22334_sie5 +
                    vimp_1102_23334_sie1 +
                    vimp_1102_23334_sie2 +
                    vimp_1102_23334_sie3 +
                    vimp_1102_23334_sie4 +
                    vimp_1102_23334_sie5 +
                    vimp_1102_28011_sie1 +
                    vimp_1102_28011_sie2 +
                    vimp_1102_28011_sie3 +
                    vimp_1102_28011_sie4 +
                    vimp_1102_28011_sie5 +
                    vimp_1102_29024_sie1 +
                    vimp_1102_29024_sie2 +
                    vimp_1102_29024_sie3 +
                    vimp_1102_29024_sie4 +
                    vimp_1102_29024_sie5 +
                    vimp_1102_29027_sie1 +
                    vimp_1102_29027_sie2 +
                    vimp_1102_29027_sie3 +
                    vimp_1102_29027_sie4 +
                    vimp_1102_29027_sie5 +
                    vimp_1102_29028_sie1 +
                    vimp_1102_29028_sie2 +
                    vimp_1102_29028_sie3 +
                    vimp_1102_29028_sie4 +
                    vimp_1102_29028_sie5


    IF ban21315_sie1 <> 1 THEN
       LET sief1 = 1
    END IF
    IF ban21315_sie2 <> 1 THEN
       LET sief2 = 2
    END IF
    IF ban21315_sie3 <> 1 THEN
       LET sief3 = 3
    END IF
    IF ban21315_sie4 <> 1 THEN
       LET sief4 = 4
    END IF
    IF ban21315_sie5 <> 1 THEN
       LET sief5 = 5
    END IF
    IF ban22332_sie1 <> 1 THEN
       LET sief1 = 1
    END IF
    IF ban22332_sie2 <> 1 THEN
       LET sief5 = 2
    END IF
    IF ban22332_sie3 <> 1 THEN
       LET sief3 = 1
    END IF
    IF ban22332_sie4 <> 1 THEN
       LET sief4 = 4
    END IF
    IF ban22332_sie5 <> 1 THEN
       LET sief5 = 5
    END IF
    IF ban23351_sie1 <> 1 THEN
       LET sief1 = 1
    END IF
    IF ban23351_sie2 <> 1 THEN
       LET sief5 = 2
    END IF
    IF ban23351_sie3 <> 1 THEN
       LET sief3 = 3
    END IF
    IF ban23351_sie4 <> 1 THEN
       LET sief4 = 4
    END IF
    IF ban23351_sie5 <> 1 THEN
       LET sief5 = 5
    END IF
    IF ban28351_sie1 <> 1 THEN
       LET sief1 = 1
    END IF
    IF ban28351_sie2 <> 1 THEN
       LET sief2 = 2
    END IF
    IF ban28351_sie3 <> 1 THEN
       LET sief3 = 3
    END IF
    IF ban28351_sie4 <> 1 THEN
       LET sief4 = 4
    END IF
    IF ban28351_sie5 <> 1 THEN
       LET sief5 = 5
    END IF

    IF ban29124_sie1 <> 1 THEN
       LET sief1 = 1
    END IF
    IF ban29124_sie2 <> 1 THEN
       LET sief2 = 2
    END IF
    IF ban29124_sie3 <> 1 THEN
       LET sief3 = 3
    END IF
    IF ban29124_sie4 <> 1 THEN
       LET sief4 = 4
    END IF
    IF ban29124_sie5 <> 1 THEN
       LET sief5 = 5
    END IF
    IF ban29127_sie1 <> 1 THEN
       LET sief1 = 1
    END IF
    IF ban29127_sie2 <> 1 THEN
       LET sief2 = 2
    END IF
    IF ban29127_sie3 <> 1 THEN
       LET sief3 = 3
    END IF
    IF ban29127_sie4 <> 1 THEN
       LET sief4 = 4
    END IF
    IF ban29127_sie5 <> 1 THEN
       LET sief5 = 5
    END IF
    IF ban29128_sie1 <> 1 THEN
       LET sief1 = 1
    END IF
    IF ban29128_sie2 <> 1 THEN
       LET sief2 = 2
    END IF
    IF ban29128_sie3 <> 1 THEN
       LET sief3 = 3
    END IF
    IF ban29128_sie4 <> 1 THEN
       LET sief4 = 4
    END IF
    IF ban29128_sie5 <> 1 THEN
       LET sief5 = 5
    END IF

         SELECT sum(importe)
         INTO importe2101_39
         FROM con_transaccion
         WHERE identificador = 1
         AND siefore IN (sief1,sief2,sief3,sief4,sief5)
         AND transaccion_cod <> 99991
         AND folio = folio39

         SELECT sum(importe)
         INTO importe2101_39_1
         FROM con_transaccion
         WHERE identificador = 1
         AND siefore = sief1
         AND transaccion_cod = 99991
         AND folio = folio39

         SELECT sum(importe)
         INTO importe2101_39_2
         FROM con_transaccion
         WHERE identificador = 1
         AND siefore = sief2
         AND transaccion_cod = 99991
         AND folio = folio39

         SELECT sum(importe)
         INTO importe2101_39_3
         FROM con_transaccion
         WHERE identificador = 1
         AND siefore = sief3
         AND transaccion_cod = 99991
         AND folio = folio39

         SELECT sum(importe)
         INTO importe2101_39_4
         FROM con_transaccion
         WHERE identificador = 1
         AND siefore = sief4
         AND transaccion_cod = 99991
         AND folio = folio39

         SELECT sum(importe)
         INTO importe2101_39_5
         FROM con_transaccion
         WHERE identificador = 1
         AND siefore = sief5
         AND transaccion_cod = 99991
         AND folio = folio39

    LET cadena = ban21315_sie1 USING "#",
                 ban21315_sie2 USING "#",
                 ban21315_sie3 USING "#",
                 ban21315_sie4 USING "#",
                 ban21315_sie5 USING "#",
                 ban22332_sie1 USING "#",
                 ban22332_sie2 USING "#",
                 ban22332_sie3 USING "#",
                 ban22332_sie4 USING "#",
                 ban22332_sie5 USING "#",
                 ban23351_sie1 USING "#",
                 ban23351_sie2 USING "#",
                 ban23351_sie3 USING "#",
                 ban23351_sie4 USING "#",
                 ban23351_sie5 USING "#",
                 ban28351_sie1 USING "#",
                 ban28351_sie2 USING "#",
                 ban28351_sie3 USING "#",
                 ban28351_sie4 USING "#",
                 ban28351_sie5 USING "#",
                 ban29124_sie1 USING "#",
                 ban29124_sie2 USING "#",
                 ban29124_sie3 USING "#",
                 ban29124_sie4 USING "#",
                 ban29124_sie5 USING "#",
                 ban29127_sie1 USING "#",
                 ban29127_sie2 USING "#",
                 ban29127_sie3 USING "#",
                 ban29127_sie4 USING "#",
                 ban29127_sie5 USING "#",
                 ban29128_sie1 USING "#",
                 ban29128_sie2 USING "#",
                 ban29128_sie3 USING "#",
                 ban29128_sie4 USING "#",
                 ban29128_sie5 USING "#"

        FOR i = 1 TO 35
            IF cadena[i] <> '1' AND
               cadena[i] <> '2' THEN
               LET ban_00039 = 0
            END IF
            IF cadena[i] <> ' ' AND
               cadena[i] <> '1' THEN
               LET ban_00039 = 1
               EXIT FOR
            END IF
        END FOR

END FUNCTION
---<erm 11 sep 2008 transei
--->erm 14 Junio 2006
FUNCTION calcula_fracc_trasp()
    
    LET vtot_09              = 0
    LET vtot_10              = 0
    LET vfracc_tot09_1_peso  = 0
    LET vfracc_tot10_1_peso  = 0
    LET vfracc_tot09_1_acc   = 0
    LET vfracc_tot10_1_acc   = 0
    LET vfracc_tot09_2_peso  = 0
    LET vfracc_tot10_2_peso  = 0
    LET vfracc_tot09_2_acc   = 0
    LET vfracc_tot10_2_acc   = 0
    LET vfracc_tot09_3_peso  = 0
    LET vfracc_tot10_3_peso  = 0
    LET vfracc_tot09_3_acc   = 0
    LET vfracc_tot10_3_acc   = 0
    LET vfracc_tot09_4_peso  = 0
    LET vfracc_tot10_4_peso  = 0
    LET vfracc_tot09_4_acc   = 0
    LET vfracc_tot10_4_acc   = 0
    LET vfracc_tot09_5_peso  = 0
    LET vfracc_tot10_5_peso  = 0
    LET vfracc_tot09_5_acc   = 0
    LET vfracc_tot10_5_acc   = 0
    LET vdif_net2            = 0
    LET vdif_net1            = 0
    LET vdif_net3            = 0
    LET vdif_net4            = 0
    LET vdif_net5            = 0
    LET vneto_fracc_peso_1   = 0
    LET vneto_fracc_peso_2   = 0
    LET vneto_fracc_peso_3   = 0
    LET vneto_fracc_peso_4   = 0
    LET vneto_fracc_peso_5   = 0
    LET vneto_fracc_acc_1    = 0
    LET vneto_fracc_acc_2    = 0
    LET vneto_fracc_acc_3    = 0
    LET vneto_fracc_acc_4    = 0
    LET vneto_fracc_acc_5    = 0
    LET ban_neteo            = 0
    LET vident09             = 0
    LET videntsie09          = 0
    LET vident10             = 0
    LET videntsie10          = 0
    LET vpesos_sie2_09       = 0
    LET vpesos_sie1_09       = 0
    LET vpesos_sie3_09       = 0
    LET vpesos_sie4_09       = 0
    LET vpesos_sie5_09       = 0
    LET vacc_sie2_09         = 0
    LET vacc_sie1_09         = 0
    LET vacc_sie3_09         = 0
    LET vacc_sie4_09         = 0
    LET vacc_sie5_09         = 0
    LET vpesos_sie2_10       = 0
    LET vpesos_sie1_10       = 0
    LET vpesos_sie3_10       = 0
    LET vpesos_sie4_10       = 0
    LET vpesos_sie5_10       = 0
    LET vacc_sie2_10         = 0
    LET vacc_sie1_10         = 0
    LET vacc_sie3_10         = 0
    LET vacc_sie4_10         = 0
    LET vacc_sie5_10         = 0
    LET vtotal_total_09      = 0
    LET vtotal_total_10      = 0

    LET ventero1  = 0
    LET vneto1    = 0
    LET vfracc1   = 0
    LET ventero2  = 0
    LET vneto2    = 0
    LET vfracc2   = 0
    LET neto_acc1 = 0
    LET neto_acc2 = 0
--->5 sie
    LET ventero3  = 0
    LET vneto3    = 0
    LET vfracc3   = 0
    LET ventero4  = 0
    LET vneto4    = 0
    LET vfracc4   = 0
    LET ventero5  = 0
    LET vneto5    = 0
    LET vfracc5   = 0
    LET neto_acc3 = 0
    LET neto_acc4 = 0
    LET neto_acc5 = 0
---<5 sie

   SELECT MAX(fecha_emision)
   INTO   vfemision09
   FROM   con_transaccion
   WHERE  proceso_cod = '00009'
   AND    estado      = 20

   SELECT MAX(fecha_emision)
   INTO   vfemision10
   FROM   con_transaccion
   WHERE  proceso_cod = '00010'
   AND    estado      = 20

--   IF vfemision09 = vfemision10 AND      -----quitar comentario para prod
--      vfemision09 = TODAY  THEN          -----quitar comentario para prod
   IF vfemision09 = vfemision10 THEN       -----comentar al momento de quitar el comentario
      LET ban_neteo = 1
      DECLARE cur_imp_09 CURSOR FOR
         SELECT SUM(importe),identificador,siefore
         FROM   con_transaccion
         WHERE  proceso_cod = '00009'
         AND    fecha_emision = vfemision09
         --AND    transaccion_cod IN(21011,22011,23011,26010,26011,26021,26110,28011,90093)
         AND    transaccion_cod NOT IN(24011,29011,29022,24351,29351,99991)
         GROUP  BY 2,3
      FOREACH cur_imp_09 INTO   vtot_09,vident09,videntsie09

         IF vident09    = 1 AND
            videntsie09 = 2 THEN
            LET vpesos_sie2_09 = vtot_09
         END IF
         IF vident09    = 1 AND
            videntsie09 = 1 THEN
            LET vpesos_sie1_09 = vtot_09
         END IF
         IF vident09    = 2 AND
            videntsie09 = 2 THEN
            LET vacc_sie2_09 = vtot_09
         END IF
         IF vident09    = 2 AND
            videntsie09 = 1 THEN
            LET vacc_sie1_09 = vtot_09
         END IF
--->5 sie
         IF vident09    = 1 AND
            videntsie09 = 3 THEN
            LET vpesos_sie3_09 = vtot_09
         END IF
         IF vident09    = 1 AND
            videntsie09 = 4 THEN
            LET vpesos_sie4_09 = vtot_09
         END IF
         IF vident09    = 2 AND
            videntsie09 = 3 THEN
            LET vacc_sie3_09 = vtot_09
         END IF
         IF vident09    = 2 AND
            videntsie09 = 4 THEN
            LET vacc_sie4_09 = vtot_09
         END IF
         IF vident09    = 1 AND
            videntsie09 = 5 THEN
            LET vpesos_sie5_09 = vtot_09
         END IF
         IF vident09    = 2 AND
            videntsie09 = 5 THEN
            LET vacc_sie5_09 = vtot_09
         END IF
---<5 sie
      END FOREACH

      SELECT SUM(importe)
      INTO   vfracc_tot09_1_peso
      FROM   con_transaccion
      WHERE  proceso_cod = '00009'
      AND    fecha_emision = vfemision09
      AND    transaccion_cod = 99991
      AND    siefore         = 1
      AND    identificador   = 1

      IF vfracc_tot09_1_peso IS NULL THEN
         LET vfracc_tot09_1_peso = 0
      END IF

      SELECT SUM(importe)
      INTO   vfracc_tot09_2_peso
      FROM   con_transaccion
      WHERE  proceso_cod = '00009'
      AND    fecha_emision = vfemision09
      AND    transaccion_cod = 99991
      AND    siefore         = 2
      AND    identificador   = 1

      IF vfracc_tot09_2_peso IS NULL THEN
         LET vfracc_tot09_2_peso = 0
      END IF

      SELECT SUM(importe)
      INTO   vfracc_tot09_1_acc
      FROM   con_transaccion
      WHERE  proceso_cod     = '00009'
      AND    fecha_emision   = vfemision09
      AND    transaccion_cod = 99991
      AND    siefore         = 1
      AND    identificador   = 2

      IF vfracc_tot09_1_acc IS NULL THEN
         LET vfracc_tot09_1_acc = 0
      END IF

      SELECT SUM(importe)
      INTO   vfracc_tot09_2_acc
      FROM   con_transaccion
      WHERE  proceso_cod     = '00009'
      AND    fecha_emision   = vfemision09
      AND    transaccion_cod = 99991
      AND    siefore         = 2
      AND    identificador   = 2

      IF vfracc_tot09_2_acc IS NULL THEN
         LET vfracc_tot09_2_acc = 0
      END IF

--->5 sief
      SELECT SUM(importe)
      INTO   vfracc_tot09_3_peso
      FROM   con_transaccion
      WHERE  proceso_cod = '00009'
      AND    fecha_emision = vfemision09
      AND    transaccion_cod = 99991
      AND    siefore         = 3
      AND    identificador   = 1

      IF vfracc_tot09_3_peso IS NULL THEN
         LET vfracc_tot09_3_peso = 0
      END IF

      SELECT SUM(importe)
      INTO   vfracc_tot09_4_peso
      FROM   con_transaccion
      WHERE  proceso_cod = '00009'
      AND    fecha_emision = vfemision09
      AND    transaccion_cod = 99991
      AND    siefore         = 4
      AND    identificador   = 1

      IF vfracc_tot09_4_peso IS NULL THEN
         LET vfracc_tot09_4_peso = 0
      END IF

      SELECT SUM(importe)
      INTO   vfracc_tot09_5_peso
      FROM   con_transaccion
      WHERE  proceso_cod = '00009'
      AND    fecha_emision = vfemision09
      AND    transaccion_cod = 99991
      AND    siefore         = 5
      AND    identificador   = 1

      IF vfracc_tot09_5_peso IS NULL THEN
         LET vfracc_tot09_5_peso = 0
      END IF

      SELECT SUM(importe)
      INTO   vfracc_tot09_3_acc
      FROM   con_transaccion
      WHERE  proceso_cod     = '00009'
      AND    fecha_emision   = vfemision09
      AND    transaccion_cod = 99991
      AND    siefore         = 3
      AND    identificador   = 2

      IF vfracc_tot09_3_acc IS NULL THEN
         LET vfracc_tot09_3_acc = 0
      END IF

      SELECT SUM(importe)
      INTO   vfracc_tot09_4_acc
      FROM   con_transaccion
      WHERE  proceso_cod     = '00009'
      AND    fecha_emision   = vfemision09
      AND    transaccion_cod = 99991
      AND    siefore         = 4
      AND    identificador   = 2

      IF vfracc_tot09_4_acc IS NULL THEN
         LET vfracc_tot09_4_acc = 0
      END IF

      SELECT SUM(importe)
      INTO   vfracc_tot09_5_acc
      FROM   con_transaccion
      WHERE  proceso_cod     = '00009'
      AND    fecha_emision   = vfemision09
      AND    transaccion_cod = 99991
      AND    siefore         = 5
      AND    identificador   = 2

      IF vfracc_tot09_5_acc IS NULL THEN
         LET vfracc_tot09_5_acc = 0
      END IF
---<5 sief

      DECLARE cur_imp_10 CURSOR FOR
         SELECT (SUM(importe)*(-1)),identificador,siefore
         FROM   con_transaccion
         WHERE  proceso_cod = '00010'
         AND    fecha_emision = vfemision10
         --AND    transaccion_cod IN(21351,22351.23351,28351,26110,26010,27351,26351,90093)
         AND    transaccion_cod NOT IN(24011,29011,29022,24351,29351,99991)
         GROUP  BY 2,3
      FOREACH cur_imp_10 INTO   vtot_10,vident10,videntsie10

         IF vident10    = 1 AND
            videntsie10 = 2 THEN
            LET vpesos_sie2_10 = vtot_10
         END IF
         IF vident10    = 1 AND
            videntsie10 = 1 THEN
            LET vpesos_sie1_10 = vtot_10
         END IF
         IF vident10    = 2 AND
            videntsie10 = 2 THEN
            LET vacc_sie2_10 = vtot_10
         END IF
         IF vident10    = 2 AND
            videntsie10 = 1 THEN
            LET vacc_sie1_10 = vtot_10
         END IF
--->5 sie
         IF vident10    = 1 AND
            videntsie10 = 3 THEN
            LET vpesos_sie3_10 = vtot_10
         END IF
         IF vident10    = 1 AND
            videntsie10 = 4 THEN
            LET vpesos_sie4_10 = vtot_10
         END IF
         IF vident10    = 2 AND
            videntsie10 = 3 THEN
            LET vacc_sie3_10 = vtot_10
         END IF
         IF vident10    = 2 AND
            videntsie10 = 4 THEN
            LET vacc_sie4_10 = vtot_10
         END IF
         IF vident10    = 1 AND
            videntsie10 = 5 THEN
            LET vpesos_sie5_10 = vtot_10
         END IF
         IF vident10    = 2 AND
            videntsie10 = 5 THEN
            LET vacc_sie5_10 = vtot_10
         END IF
---<5 sie
      END FOREACH

--      SELECT (SUM(importe)*(-1))
      SELECT SUM(importe)
      INTO   vfracc_tot10_1_peso
      FROM   con_transaccion
      WHERE  proceso_cod     = '00010'
      AND    fecha_emision   = vfemision09
      AND    transaccion_cod = 99991
      AND    siefore         = 1
      AND    identificador   = 1

      IF vfracc_tot10_1_peso IS NULL THEN
         LET vfracc_tot10_1_peso = 0
      END IF

--      SELECT (SUM(importe)*(-1))
      SELECT SUM(importe)
      INTO   vfracc_tot10_2_peso
      FROM   con_transaccion
      WHERE  proceso_cod     = '00010'
      AND    fecha_emision   = vfemision09
      AND    transaccion_cod = 99991
      AND    siefore         = 2
      AND    identificador   = 1

      IF vfracc_tot10_2_peso IS NULL THEN
         LET vfracc_tot10_2_peso = 0
      END IF

      --SELECT (SUM(importe)*(-1))
      SELECT SUM(importe)
      INTO   vfracc_tot10_1_acc
      FROM   con_transaccion
      WHERE  proceso_cod     = '00010'
      AND    fecha_emision   = vfemision09
      AND    transaccion_cod = 99991
      AND    siefore         = 1
      AND identificador      = 2

      IF vfracc_tot10_1_acc IS NULL THEN
         LET vfracc_tot10_1_acc = 0
      END IF

--      SELECT (SUM(importe)*(-1))
      SELECT SUM(importe)
      INTO   vfracc_tot10_2_acc
      FROM   con_transaccion
      WHERE  proceso_cod     = '00010'
      AND    fecha_emision   = vfemision09
      AND    transaccion_cod = 99991
      AND    siefore         = 2
      AND identificador      = 2

      IF vfracc_tot10_2_acc IS NULL THEN
         LET vfracc_tot10_2_acc = 0
      END IF

---> 5 sie
--      SELECT (SUM(importe)*(-1))
      SELECT SUM(importe)
      INTO   vfracc_tot10_3_peso
      FROM   con_transaccion
      WHERE  proceso_cod     = '00010'
      AND    fecha_emision   = vfemision09
      AND    transaccion_cod = 99991
      AND    siefore         = 3
      AND    identificador   = 1

      IF vfracc_tot10_3_peso IS NULL THEN
         LET vfracc_tot10_3_peso = 0
      END IF

--      SELECT (SUM(importe)*(-1))
      SELECT SUM(importe)
      INTO   vfracc_tot10_4_peso
      FROM   con_transaccion
      WHERE  proceso_cod     = '00010'
      AND    fecha_emision   = vfemision09
      AND    transaccion_cod = 99991
      AND    siefore         = 4
      AND    identificador   = 1

      IF vfracc_tot10_4_peso IS NULL THEN
         LET vfracc_tot10_4_peso = 0
      END IF

--      SELECT (SUM(importe)*(-1))
      SELECT SUM(importe)
      INTO   vfracc_tot10_5_peso
      FROM   con_transaccion
      WHERE  proceso_cod     = '00010'
      AND    fecha_emision   = vfemision09
      AND    transaccion_cod = 99991
      AND    siefore         = 5
      AND    identificador   = 1

      IF vfracc_tot10_5_peso IS NULL THEN
         LET vfracc_tot10_5_peso = 0
      END IF

      --SELECT (SUM(importe)*(-1))
      SELECT SUM(importe)
      INTO   vfracc_tot10_3_acc
      FROM   con_transaccion
      WHERE  proceso_cod     = '00010'
      AND    fecha_emision   = vfemision09
      AND    transaccion_cod = 99991
      AND    siefore         = 3
      AND identificador      = 2

      IF vfracc_tot10_3_acc IS NULL THEN
         LET vfracc_tot10_3_acc = 0
      END IF

--      SELECT (SUM(importe)*(-1))
      SELECT SUM(importe)
      INTO   vfracc_tot10_4_acc
      FROM   con_transaccion
      WHERE  proceso_cod     = '00010'
      AND    fecha_emision   = vfemision09
      AND    transaccion_cod = 99991
      AND    siefore         = 4
      AND identificador      = 2

      IF vfracc_tot10_4_acc IS NULL THEN
         LET vfracc_tot10_4_acc = 0
      END IF

--      SELECT (SUM(importe)*(-1))
      SELECT SUM(importe)
      INTO   vfracc_tot10_5_acc
      FROM   con_transaccion
      WHERE  proceso_cod     = '00010'
      AND    fecha_emision   = vfemision09
      AND    transaccion_cod = 99991
      AND    siefore         = 5
      AND identificador      = 2

      IF vfracc_tot10_5_acc IS NULL THEN
         LET vfracc_tot10_5_acc = 0
      END IF
---<5 sie

   END IF
{
      IF vfracc_tot10_1_acc IS NULL THEN
         LET vfracc_tot10_1_acc = 0
      END IF
}
   IF vpesos_sie2_09 > vpesos_sie2_10 THEN
      LET vdif_net2 = vpesos_sie2_09 - vpesos_sie2_10
   ELSE
      LET vdif_net2 = vpesos_sie2_10 - vpesos_sie2_09
   END IF

   IF vpesos_sie1_09 > vpesos_sie1_10 THEN
      LET vdif_net1 = vpesos_sie1_09 - vpesos_sie1_10
   ELSE
      LET vdif_net1 = vpesos_sie1_10 - vpesos_sie1_09
   END IF

--->5 sie
   IF vpesos_sie3_09 > vpesos_sie3_10 THEN
      LET vdif_net3 = vpesos_sie3_09 - vpesos_sie3_10
   ELSE
      LET vdif_net3 = vpesos_sie3_10 - vpesos_sie3_09
   END IF

   IF vpesos_sie4_09 > vpesos_sie4_10 THEN
      LET vdif_net4 = vpesos_sie4_09 - vpesos_sie4_10
   ELSE
      LET vdif_net4 = vpesos_sie4_10 - vpesos_sie4_09
   END IF

   IF vpesos_sie5_09 > vpesos_sie5_10 THEN
      LET vdif_net5 = vpesos_sie5_09 - vpesos_sie5_10
   ELSE
      LET vdif_net5 = vpesos_sie5_10 - vpesos_sie5_09
   END IF
---<5 sie

   LET vtotal_total_09 = vpesos_sie2_09 + vpesos_sie1_09 + vpesos_sie3_09 + vpesos_sie4_09 + vpesos_sie5_09
   LET vtotal_total_10 = vpesos_sie2_10 + vpesos_sie1_10 + vpesos_sie3_10 + vpesos_sie4_10 + vpesos_sie5_10
{--- se descomenta el 03 marzo 2008
   IF vfracc_tot09_1_peso > vfracc_tot10_1_peso THEN
      LET vneto_fracc_peso_1 = vfracc_tot09_1_peso - vfracc_tot10_1_peso
   ELSE 
      LET vneto_fracc_peso_1 = vfracc_tot10_1_peso - vfracc_tot09_1_peso
   END IF

   IF vfracc_tot09_2_peso > vfracc_tot10_2_peso THEN
      LET vneto_fracc_peso_2 = vfracc_tot09_2_peso - vfracc_tot10_2_peso
   ELSE 
      LET vneto_fracc_peso_2 = vfracc_tot10_2_peso - vfracc_tot09_2_peso
   END IF

   IF vfracc_tot09_1_acc > vfracc_tot10_1_acc THEN
      LET vneto_fracc_acc_1 = vfracc_tot09_1_acc - vfracc_tot10_1_acc
   ELSE 
      LET vneto_fracc_acc_1 = vfracc_tot10_1_acc - vfracc_tot09_1_acc
   END IF

   IF vfracc_tot09_2_acc > vfracc_tot10_2_acc THEN
      LET vneto_fracc_acc_2 = vfracc_tot09_2_acc - vfracc_tot10_2_acc
   ELSE 
      LET vneto_fracc_acc_2 = vfracc_tot10_2_acc - vfracc_tot09_2_acc
   END IF
---}
--- se descomenta el 03 marzo 2008

   IF vtotal_total_09 > vtotal_total_10 THEN
      LET neto_acc1         = vacc_sie1_09 - vacc_sie1_10
      LET ventero1          = neto_acc1
      LET vfracc1           = neto_acc1 - ventero1
      LET vneto1            = 1 - vfracc1
      LET neto_acc2         = vacc_sie2_09 - vacc_sie2_10
      LET ventero2          = neto_acc2
      LET vfracc2           = neto_acc2 - ventero2
      LET vneto2            = 1 - vfracc2
--->5 sie
      LET neto_acc3         = vacc_sie3_09 - vacc_sie3_10
      LET ventero3          = neto_acc3
      LET vfracc3           = neto_acc3 - ventero3
      LET vneto3            = 1 - vfracc3
      LET neto_acc4         = vacc_sie4_09 - vacc_sie4_10
      LET ventero4          = neto_acc4
      LET vfracc4           = neto_acc4 - ventero4
      LET vneto4            = 1 - vfracc4
      LET neto_acc5         = vacc_sie5_09 - vacc_sie5_10
      LET ventero5          = neto_acc5
      LET vfracc5           = neto_acc5 - ventero5
      LET vneto5            = 1 - vfracc5
---<5 sie
   ELSE 
      LET neto_acc1         = vacc_sie1_10 - vacc_sie1_09
      LET ventero1          = neto_acc1
      LET vfracc1           = neto_acc1 - ventero1
      LET vneto1            = 1 - vfracc1
      LET neto_acc2         = vacc_sie2_10 - vacc_sie2_09
      LET ventero2          = neto_acc2
      LET vfracc2           = neto_acc2 - ventero2
      LET vneto2            = 1 - vfracc2
---<5 sie
      LET neto_acc3         = vacc_sie3_10 - vacc_sie3_09
      LET ventero3          = neto_acc3
      LET vfracc3           = neto_acc3 - ventero3
      LET vneto3            = 1 - vfracc3
      LET neto_acc4         = vacc_sie4_10 - vacc_sie4_09
      LET ventero4          = neto_acc4
      LET vfracc4           = neto_acc4 - ventero4
      LET vneto4            = 1 - vfracc4
      LET neto_acc5         = vacc_sie5_10 - vacc_sie5_09
      LET ventero5          = neto_acc5
      LET vfracc5           = neto_acc5 - ventero5
      LET vneto5            = 1 - vfracc5
---<5 se
   END IF


---}

---<
END FUNCTION
FUNCTION consulta()
#c-----------------
    DEFINE #loc #integer
        arr_c                 ,
        flag                  ,
        i                     INTEGER
    DEFINE x_busca        CHAR(100)
    DEFINE txt_2          CHAR(500)
    DEFINE arr_2  ARRAY[2000] OF RECORD
        fecha_emision        DATE,
        proceso_cod          CHAR(05),
        desc_proceso         CHAR(20),
        estado               SMALLINT,
        desc_estado          CHAR(10),
        identificador        SMALLINT,
        desc_identificador   CHAR(6),
        siefore          SMALLINT,
        siefore_desc         CHAR(08),
        importe              DECIMAL(15,2)
    END RECORD
    DEFINE lmovimiento       SMALLINT

    INITIALIZE x_busca TO NULL
    CLEAR FORM

    DISPLAY "    Utilice las flechas para navegar     Control [V] para ver detalle    " AT 5,1
    DISPLAY "  FECHA       P R O C E S O    ESTADO      TIPO    SIEFORE     I M P O R T E   " AT 6,1 ATTRIBUTE(REVERSE)
    DISPLAY "  EMISION    TIPO   DESCRIPCION                                  PESOS / ACC    " AT 7,1 ATTRIBUTE(REVERSE)

    LET int_flag              = FALSE
    
    CONSTRUCT BY NAME x_busca ON fecha_emision,
                                 proceso_cod,
                                 estado,
                                 siefore

        ON KEY (CONTROL-C)
                LET int_flag=TRUE
                EXIT CONSTRUCT

        ON KEY (INTERRUPT) 
                LET int_flag=TRUE
                EXIT CONSTRUCT

        ON KEY (Esc)
            LET int_flag = FALSE
            EXIT CONSTRUCT

    END CONSTRUCT
      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         DISPLAY "                                                                               " AT 5,1
         DISPLAY "                                                                               " AT 6,1
         DISPLAY "                                                                               " AT 7,1
         RETURN
      END IF


    LET txt_2 =" SELECT a.fecha_emision, ",
               "a.proceso_cod, ",
               "' ', ",   #desc_proceso
               "a.estado, ",
               "' ', ",   #desc_estado
               "a.identificador, ",
               "' ', ",   #desc_identificador
               "a.siefore, ",
               "' ', ",   #desc_siefore
               "0 ",
               " FROM   con_transaccion a ",
               " WHERE  ",x_busca CLIPPED,
               " GROUP BY 1,2,4,6,8 ",
	       " ORDER BY 2,8,6 "

    PREPARE pre_1 FROM txt_2
    DECLARE cur_1 CURSOR FOR pre_1

    LET i = 1
    FOREACH cur_1 INTO arr_2[i].*
        IF STATUS <> NOTFOUND THEN

	    SELECT a.descripcion,
                   a.movimiento
	    INTO   arr_2[i].desc_proceso,
                   lmovimiento
	    FROM   tab_proceso a
	    WHERE  a.proceso_cod = arr_2[i].proceso_cod

	    SELECT a.descripcion
	    INTO   arr_2[i].desc_estado
	    FROM   con_status a
	    WHERE  a.estado = arr_2[i].estado

            SELECT a.razon_social
            INTO   arr_2[i].siefore_desc
            FROM   tab_siefore_local a
            WHERE  codigo_siefore = arr_2[i].siefore         

	    IF arr_2[i].identificador = 1 THEN
                LET arr_2[i].desc_identificador = "PESOS"
            ELSE
                LET arr_2[i].desc_identificador = "ACCION"
            END IF

	    IF lmovimiento = -1 THEN

                SELECT SUM(importe)
                INTO   arr_2[i].importe
                FROM   con_transaccion
                WHERE  fecha_emision = arr_2[i].fecha_emision
                AND    identificador = arr_2[i].identificador
                AND    proceso_cod   = arr_2[i].proceso_cod
                AND    siefore       = arr_2[i].siefore
                {AND    transaccion_cod NOT IN(
                                      SELECT transaccion_cod
                                      FROM   tab_transaccion
                                      WHERE  descripcion_1 MATCHES "*VIV*")}
            ELSE
                SELECT SUM(importe)
                INTO   arr_2[i].importe
                FROM   con_transaccion
                WHERE  fecha_emision = arr_2[i].fecha_emision
                AND    identificador = arr_2[i].identificador
                AND    proceso_cod   = arr_2[i].proceso_cod
                AND    siefore       = arr_2[i].siefore
                {AND    transaccion_cod NOT IN(
                                      SELECT transaccion_cod
                                      FROM   tab_transaccion
                                      WHERE  descripcion_1 MATCHES "*VIV*")}
            END IF

        END IF

        LET i = i + 1
    END FOREACH
    
    IF i = 1 THEN
        CLEAR FORM
        ERROR "    NO EXISTE REGISTRO " ATTRIBUTE(NORMAL)
        RETURN
    END IF

    CALL SET_COUNT(i-1)
    DISPLAY ARRAY arr_2 TO scr_2.*
        ON KEY ( control-v )
            LET i = ARR_CURR()
	    IF arr_2[i].proceso_cod >= "00001" AND
	       arr_2[i].proceso_cod <= "00004" THEN
                CALL detalle_registro1(arr_2[i].fecha_emision,
                                       arr_2[i].identificador,
                                       arr_2[i].proceso_cod,
                                       arr_2[i].siefore)
	    ELSE
                CALL detalle_registro(arr_2[i].fecha_emision,
                                      arr_2[i].identificador,
                                      arr_2[i].proceso_cod,
                                      arr_2[i].siefore)
	    END IF

        ON KEY ( control-c )
            FOR i = 1 TO 6
               DISPLAY arr_2[i].* TO scr_2[i].*
            END FOR
            EXIT DISPLAY
    END DISPLAY
	  
         DISPLAY "                                                                               " AT 5,1
         DISPLAY "                                                                               " AT 6,1
         DISPLAY "                                                                               " AT 7,1
END FUNCTION
FUNCTION detalle_registro(reg_2)
#dr-----------------------------
    DEFINE #loc #integer
        arr_c                ,
        flag                 ,
        i                    INTEGER
    DEFINE x_busca           CHAR(100)
    DEFINE txt_2             CHAR(500)
    DEFINE aux_pausa         CHAR(01)
    DEFINE arc_2             SMALLINT
    DEFINE vimporte          DECIMAL(15,2)
    DEFINE ximporte          DECIMAL(15,2)
    DEFINE scr_2             SMALLINT
    DEFINE reg_2  RECORD
        fecha_emision        DATE,
        identificador        SMALLINT,
        proceso_cod          CHAR(05),
        siefore              SMALLINT
    END RECORD
    DEFINE arr_3  ARRAY[200] OF RECORD
        transaccion_cod      INTEGER,
        desc_transaccion     CHAR(33),
        estado               SMALLINT,
        desc_estado          CHAR(10),
        siefore          SMALLINT,
        siefore_desc         CHAR(08),
	folio                INTEGER,
        importe              DECIMAL(15,2)
    END RECORD
    DEFINE desc_id     CHAR(8)

    INITIALIZE x_busca TO NULL
    INITIALIZE arr_3 TO NULL
    INITIALIZE scr_2 TO NULL

    OPEN WINDOW conm0012 AT 5,2 WITH FORM "CONM0012" ATTRIBUTE(BORDER)
    DISPLAY "    Flechas para navegar      [Esc] Para grabar      [Crtl-B] Para agrupar     " AT 1,1
    DISPLAY "               D E T A L L E   D E   M O V I M I E N T O S                     " AT  2,1 ATTRIBUTE(REVERSE)
    DISPLAY "     T R A N S A C C I O N      ESTADO     SIEFORE   I M P O R T E    " AT  3,1 ATTRIBUTE(REVERSE)

    LET int_flag              = FALSE
    
    DECLARE cur_3 CURSOR FOR
    SELECT a.transaccion_cod,
           ' ',   #desc_proceso
           a.estado,
           ' ',   #desc_estado
           a.siefore,
           ' ',   #desc_siefore
           folio,
           importe
    FROM  con_transaccion a
    WHERE a.fecha_emision = reg_2.fecha_emision 
    AND   a.identificador = reg_2.identificador
    AND   a.proceso_cod   = reg_2.proceso_cod
    AND   a.siefore       = reg_2.siefore
    GROUP BY 1,3,5,7,8
    ORDER BY 7,1,5

    LET i = 1
    FOREACH cur_3 INTO arr_3[i].*

        IF STATUS <> NOTFOUND THEN

	    SELECT a.descripcion
	    INTO   arr_3[i].desc_estado
	    FROM   con_status a
	    WHERE  a.estado = arr_3[i].estado

	    SELECT a.descripcion_1
	    INTO   arr_3[i].desc_transaccion
	    FROM   tab_transaccion a
	    WHERE  a.transaccion_cod = arr_3[i].transaccion_cod
            AND    a.proceso_cod     = reg_2.proceso_cod

            SELECT a.razon_social
            INTO   arr_3[i].siefore_desc
            FROM   tab_siefore_local a
            WHERE  codigo_siefore = reg_2.siefore

	    IF reg_2.identificador = 1 THEN
                LET desc_id = "PESOS"
            ELSE
                LET desc_id = "ACCIONES"
            END IF

        END IF
        DISPLAY BY NAME desc_id
        LET i = i + 1
    END FOREACH
    
    IF i = 1 THEN
        CLEAR FORM
        ERROR "    NO EXISTE REGISTRO " ATTRIBUTE(NORMAL)
        CLOSE WINDOW conm0012
        RETURN
    END IF

    CALL SET_COUNT(i-1)
    INPUT ARRAY arr_3 WITHOUT DEFAULTS FROM scr_3.*
        BEFORE FIELD importe
        LET arc_2 = ARR_CURR()
        LET scr_2 = SCR_LINE()

        LET vimporte = arr_3[arc_2].importe
        AFTER FIELD importe
            IF  arr_3[arc_2].estado <> 40 THEN
                IF arr_3[arc_2].importe <> vimporte THEN
                    WHILE TRUE
                        PROMPT "Desea Modificar el Registro S/N ? "
                        FOR CHAR aux_pausa
                        IF aux_pausa MATCHES "[SsNn]" THEN
                            EXIT WHILE
                        END IF
                    END WHILE
                    IF aux_pausa MATCHES "[Ss]" THEN
		        IF vimporte < 0 THEN
			    IF arr_3[arc_2].importe > 0 THEN
			        LET arr_3[arc_2].importe = arr_3[arc_2].importe
							   * -1
			     END IF
		         END IF
		         IF vimporte > 0 THEN
			     IF arr_3[arc_2].importe < 0 THEN
			        LET arr_3[arc_2].importe = arr_3[arc_2].importe
							   * -1
			     END IF
		         END IF

                         IF (reg_2.proceso_cod = "00022" OR
                             reg_2.proceso_cod = "00027" )THEN
                            UPDATE con_transaccion
                            SET   importe         = arr_3[arc_2].importe
                            WHERE fecha_emision   = reg_2.fecha_emision
                            AND   identificador   = reg_2.identificador
                            AND   proceso_cod     = reg_2.proceso_cod
                            AND   transaccion_cod = arr_3[arc_2].transaccion_cod
                            AND   siefore         = reg_2.siefore
                            AND   importe         = vimporte
			 ELSE
                            UPDATE con_transaccion
                            SET   importe         = arr_3[arc_2].importe
                            WHERE fecha_emision   = reg_2.fecha_emision
                            AND   identificador   = reg_2.identificador
                            AND   proceso_cod     = reg_2.proceso_cod
                            AND   transaccion_cod = arr_3[arc_2].transaccion_cod
                            AND   siefore         = reg_2.siefore
                            AND   folio           = arr_3[arc_2].folio
                            AND   importe         = vimporte
			 END IF

                         IF (reg_2.proceso_cod = "00009" 
		         OR  reg_2.proceso_cod = "00010") THEN
                             IF (arr_3[arc_2].transaccion_cod <> 24011
			     OR  arr_3[arc_2].transaccion_cod <> 29011
			     OR  arr_3[arc_2].transaccion_cod <> 24351
			     OR  arr_3[arc_2].transaccion_cod <> 29351
			     OR  arr_3[arc_2].transaccion_cod <> 99991) THEN

				 SELECT "X"
				 FROM   con_transaccion
				 WHERE  proceso_cod = "00028"
				 AND    fecha_emision = reg_2.fecha_emision
				 AND    identificador = reg_2.identificador
                                 AND    siefore       = reg_2.siefore
				 GROUP BY 1
				 IF STATUS <> NOTFOUND THEN
			             LET ximporte = ""
				     SELECT sum(importe)
				     INTO   ximporte
				     FROM   con_transaccion
				     WHERE  proceso_cod in("00009","00010")
				     AND    fecha_emision = reg_2.fecha_emision
				     AND    identificador = reg_2.identificador
				     AND    transaccion_cod NOT IN(24011,
					     29011,24351,29351,99991)
                                     AND    siefore = reg_2.siefore 
				     AND    estado = 20

			             UPDATE con_transaccion
			             SET    importe = ximporte
			             WHERE  proceso_cod = "00028"
			             AND    fecha_emision = reg_2.fecha_emision
			             AND    identificador = reg_2.identificador
                                     AND    siefore       = reg_2.siefore

                                     ERROR "REGISTRO MODIFICADO DEL NETEO"
                                     SLEEP 2
                                     ERROR ""
                                 END IF
                             END IF
                         END IF
                         ERROR "REGISTRO MODIFICADO"
                         SLEEP 2
                         ERROR ""
                     ELSE
                         ERROR "MODIFICACION CANCELADA"
                         SLEEP 2
                         ERROR ""
                     END IF
                 END IF
            ELSE
                ERROR "Registro contabilizado no es susceptible a modificar" 
                SLEEP 1
            END IF

        ON KEY ( control-b )
           CALL engloba_detalle(reg_2.*)
            
        ON KEY ( control-c )
            INITIALIZE arr_3 TO NULL
            FOR i = 1 TO 6
               DISPLAY arr_3[i].* TO scr_3[i].*
            END FOR
            EXIT INPUT

        ON KEY ( INTERRUPT )
            EXIT INPUT

    END INPUT

    CLOSE WINDOW conm0012
END FUNCTION

FUNCTION detalle_registro1(reg_2)
#dr1-----------------------------
    DEFINE #loc #integer
        arr_c                ,
        flag                 ,
        i                    INTEGER
    DEFINE x_busca           CHAR(100)
    DEFINE txt_2             CHAR(500)
    DEFINE aux_pausa         CHAR(01)
    DEFINE arc_2             SMALLINT
    DEFINE vimporte          DECIMAL(15,2)
    DEFINE ximporte          DECIMAL(15,2)
    DEFINE xmonto_tot        DECIMAL(15,2)
    DEFINE scr_2             SMALLINT
    DEFINE reg_2  RECORD
        fecha_emision        DATE,
        identificador        SMALLINT,
        proceso_cod          CHAR(05),
        siefore          SMALLINT
    END RECORD
    DEFINE arr_3  ARRAY[200] OF RECORD
        transaccion_cod      INTEGER,
        desc_transaccion     CHAR(33),
        estado               SMALLINT,
        desc_estado          CHAR(10),
        siefore          SMALLINT,
        siefore_desc         CHAR(08),
	folio                INTEGER,
        importe              DECIMAL(15,2)
    END RECORD
    DEFINE desc_id     CHAR(8)

    INITIALIZE x_busca TO NULL
    INITIALIZE arr_3 TO NULL
    INITIALIZE scr_2 TO NULL

    OPEN WINDOW conm0012a AT 5,2 WITH FORM "CONM0012A" ATTRIBUTE(BORDER)
    DISPLAY "    Flechas para navegar      [Esc] Para grabar      [Crtl-B] Para agrupar     " AT 1,1
    DISPLAY "               D E T A L L E   D E   M O V I M I E N T O S                     " AT  2,1 ATTRIBUTE(REVERSE)
    DISPLAY "     T R A N S A C C I O N           ESTADO   SIEFORE          I M P O R T E    " AT  3,1 ATTRIBUTE(REVERSE)

    LET int_flag              = FALSE
    
    DECLARE cur_5 CURSOR FOR

        SELECT a.transaccion_cod,
               ' ',   #desc_proceso
               a.estado,
               ' ',   #desc_estado
               a.siefore,
               ' ',   #desc_siefore
               folio,
               importe
        FROM  con_transaccion a
        WHERE a.fecha_emision = reg_2.fecha_emision 
        AND   a.identificador = reg_2.identificador
        AND   a.proceso_cod   = reg_2.proceso_cod
        AND   a.siefore       = reg_2.siefore
        GROUP BY 1,3,5,7,8
        ORDER BY 7,5,1

    LET i = 1
    LET xmonto_tot = 0
    FOREACH cur_5 INTO arr_3[i].*
        IF STATUS <> NOTFOUND THEN

            SELECT a.descripcion
            INTO   arr_3[i].desc_estado
            FROM   con_status a
            WHERE  a.estado = arr_3[i].estado

            SELECT a.descripcion_1
            INTO   arr_3[i].desc_transaccion
            FROM   tab_transaccion a
            WHERE  a.transaccion_cod = arr_3[i].transaccion_cod
            AND    a.proceso_cod     = reg_2.proceso_cod

            SELECT a.razon_social
            INTO   arr_3[i].siefore_desc
            FROM   tab_siefore_local a
            WHERE  codigo_siefore = reg_2.siefore

	         IF reg_2.identificador = 1 THEN
                LET desc_id = "PESOS"
            ELSE
                LET desc_id = "ACCIONES"
            END IF

           LET xmonto_tot = xmonto_tot + arr_3[i].importe

        END IF

        DISPLAY BY NAME desc_id
        LET i = i + 1
    END FOREACH
    
    IF i = 1 THEN
        CLEAR FORM
        ERROR "    NO EXISTE REGISTRO " ATTRIBUTE(NORMAL)
        CLOSE WINDOW conm0012a
        RETURN
    END IF

    CALL SET_COUNT(i-1)
    INPUT ARRAY arr_3 WITHOUT DEFAULTS FROM scr_3.*
        BEFORE FIELD importe
        LET arc_2 = ARR_CURR()
        LET scr_2 = SCR_LINE()

        LET vimporte = arr_3[arc_2].importe
	DISPLAY BY NAME xmonto_tot
        AFTER FIELD importe
            IF  arr_3[arc_2].estado <> 40 THEN
                 IF arr_3[arc_2].importe <> vimporte THEN
                    WHILE TRUE
                        PROMPT "Desea Modificar el Registro S/N ? "
                        FOR CHAR aux_pausa
                        IF aux_pausa MATCHES "[SsNn]" THEN
                            EXIT WHILE
                        END IF
                    END WHILE
                     IF aux_pausa MATCHES "[Ss]" THEN
                       IF vimporte < 0 THEN
                           IF arr_3[arc_2].importe > 0 THEN
                               LET arr_3[arc_2].importe = arr_3[arc_2].importe
                                   * -1
                           END IF
                       END IF
                           IF vimporte > 0 THEN
                               IF arr_3[arc_2].importe < 0 THEN
                                    LET arr_3[arc_2].importe = arr_3[arc_2].importe
                                        * -1
                               END IF
                           END IF

                           IF (reg_2.proceso_cod >= '00001'AND reg_2.proceso_cod <= '00004') AND 
                              arr_3[arc_2].folio IS NULL THEN
                              UPDATE con_transaccion
                              SET    importe         = arr_3[arc_2].importe
                              WHERE  fecha_emision   = reg_2.fecha_emision
                              AND    identificador   = reg_2.identificador
                              AND    proceso_cod     = reg_2.proceso_cod
                              AND    transaccion_cod = arr_3[arc_2].transaccion_cod
                              AND    siefore         = arr_3[arc_2].siefore
                              --AND    folio           = arr_3[arc_2].folio
                              AND    importe         = vimporte
                           ELSE
                              UPDATE con_transaccion
                              SET    importe         = arr_3[arc_2].importe
                              WHERE  fecha_emision   = reg_2.fecha_emision
                              AND    identificador   = reg_2.identificador
                              AND    proceso_cod     = reg_2.proceso_cod
                              AND    transaccion_cod = arr_3[arc_2].transaccion_cod
                              AND    siefore         = arr_3[arc_2].siefore
                              AND    folio           = arr_3[arc_2].folio
                              AND    importe         = vimporte
                           END IF

                         LET xmonto_tot = xmonto_tot + 
                            (arr_3[arc_2].importe - vimporte)

                         DISPLAY BY NAME xmonto_tot

                         ERROR "REGISTRO MODIFICADO"
                         SLEEP 2
                         ERROR ""
                     ELSE
                         ERROR "MODIFICACION CANCELADA"
                         SLEEP 2
                         ERROR ""
                     END IF
                 END IF
            ELSE
                ERROR "Registro contabilizado no es susceptible a modificar" 
                SLEEP 1
            END IF

        ON KEY ( control-b )
           CALL engloba_detalle(reg_2.*)
            
        ON KEY ( control-c )
            INITIALIZE arr_3 TO NULL
            FOR i = 1 TO 6
               DISPLAY arr_3[i].* TO scr_3[i].*
            END FOR
            EXIT INPUT

        ON KEY ( INTERRUPT )
            EXIT INPUT
    END INPUT

    CLOSE WINDOW conm0012a
END FUNCTION

FUNCTION engloba_detalle(reg_2)
#dr1-----------------------------
    DEFINE #loc #integer
        arr_c                ,
        flag                 ,
        i                    INTEGER
    DEFINE x_busca           CHAR(100)
    DEFINE txt_2             CHAR(500)
    DEFINE aux_pausa         CHAR(01)
    DEFINE arc_2             SMALLINT
    DEFINE vimporte          DECIMAL(15,2)
    DEFINE ximporte          DECIMAL(15,2)
    DEFINE xmonto_tot        DECIMAL(15,2)
    DEFINE scr_2             SMALLINT
    DEFINE reg_2  RECORD
        fecha_emision        DATE,
        identificador        SMALLINT,
        proceso_cod          CHAR(05),
        siefore              SMALLINT
    END RECORD
    DEFINE arr_3  ARRAY[200] OF RECORD
        transaccion_cod      INTEGER,
        desc_transaccion     CHAR(36),
        siefore          SMALLINT,
        siefore_desc         CHAR(08),
        importe              DECIMAL(15,2)
    END RECORD

    INITIALIZE x_busca TO NULL
    INITIALIZE arr_3 TO NULL
    INITIALIZE scr_2 TO NULL

    OPEN WINDOW conm0015 AT 10,2 WITH FORM "CONM0015" --ATTRIBUTE(BORDER)
    DISPLAY "           T R A N S A C C I O N                              I M P O R T E    " AT 1,1
    LET int_flag              = FALSE
    
    DECLARE cur_6 CURSOR FOR

        SELECT a.transaccion_cod,
               ' ',   #desc_proceso
               a.siefore,
               ' ',   #desc_siefore
               SUM(importe)
        FROM  con_transaccion a
        WHERE a.fecha_emision = reg_2.fecha_emision 
        AND   a.identificador = reg_2.identificador
        AND   a.proceso_cod   = reg_2.proceso_cod
        AND   a.siefore       = reg_2.siefore
        GROUP BY 1,3
        ORDER BY 1

    LET i = 1
    LET xmonto_tot = 0

    FOREACH cur_6 INTO arr_3[i].*
        IF STATUS <> NOTFOUND THEN
	    SELECT a.descripcion_1
	    INTO   arr_3[i].desc_transaccion
	    FROM   tab_transaccion a
	    WHERE  a.transaccion_cod = arr_3[i].transaccion_cod
            AND    a.proceso_cod     = reg_2.proceso_cod
            GROUP BY 1
            ORDER BY 1

            SELECT a.razon_social
            INTO   arr_3[i].siefore_desc
            FROM   tab_siefore_local a
            WHERE  codigo_siefore = reg_2.siefore

        END IF

        LET i = i + 1
    END FOREACH
    
    IF i = 1 THEN
        CLEAR FORM
        ERROR "    NO EXISTE REGISTRO " ATTRIBUTE(NORMAL)
        CLOSE WINDOW conm0015
        RETURN
    END IF

    CALL SET_COUNT(i-1)
    DISPLAY ARRAY arr_3 TO scr_3.*
        ON KEY ( control-c )
            INITIALIZE arr_3 TO NULL
            FOR i = 1 TO 6
               DISPLAY arr_3[i].* TO scr_3[i].*
            END FOR
            EXIT DISPLAY

        ON KEY ( INTERRUPT )
            EXIT DISPLAY
    END DISPLAY
				
    CLOSE WINDOW conm0015

END FUNCTION
FUNCTION despliega_unificados(vfolio,nss_u,vestado)
#dt-------------------------

    DEFINE l_reg ARRAY[10] OF RECORD
           nss_cta1       CHAR(11),
           folio          INTEGER,
           folio_liquida  INTEGER,
           cve_ent_cta1   CHAR(3),
           nombre         CHAR(50),
           estado    	  SMALLINT
    END RECORD

    DEFINE vestado   INTEGER
    DEFINE vfolio    INTEGER
    DEFINE nss_u     CHAR(11)
    DEFINE vpaterno  CHAR(40)
    DEFINE vmaterno  CHAR(40)
    DEFINE vnombre   CHAR(40)
    DEFINE i   	   SMALLINT

    OPEN WINDOW unim0013 AT 5,2 WITH FORM "UNIM0013" ATTRIBUTE(BORDER)
    #DISPLAY "       NSS UNIFICADOS                  " AT 1,1 
    DISPLAY "      NSS          FOLIO       AFORE           NOMBRE                 ESTADO   " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "  UNIFICADOS  NOTIFICA LIQUIDA                COMPLETO                         " AT 2,1 ATTRIBUTE(REVERSE)

   DECLARE cursor_1 CURSOR FOR
       SELECT nss_cta1,
              folio,
              folio_liquida,
              cve_ent_cta1,
              "",
              estado
       FROM   uni_unificado
       WHERE  nss_uni = nss_u
       AND    estado  = vestado
       AND    folio   = vfolio 

   LET i = 1
   FOREACH cursor_1 INTO l_reg[i].*
       SELECT paterno_cta1,
              materno_cta1,
              nombre_cta1
       INTO   vpaterno,
              vmaterno,
              vnombre
       FROM   uni_unificado
       WHERE  nss_cta1 = l_reg[i].nss_cta1
       AND    nss_uni = nss_u
       AND    estado  = vestado
       AND    folio   = vfolio 
      
       LET l_reg[i].nombre = vpaterno CLIPPED," ",vmaterno CLIPPED," ",
                             vnombre CLIPPED
       LET i = i + 1
   END FOREACH
   ERROR ""
   CALL SET_COUNT(i-1)
   DISPLAY ARRAY l_reg TO scr_3.*
      ON KEY ( control-m )
         INITIALIZE l_reg TO NULL
         FOR i = 1 TO 3
             DISPLAY l_reg[i].* TO scr_3[i].*
         END FOR
         EXIT DISPLAY
   END DISPLAY
   CLOSE WINDOW unim0013
END FUNCTION #despliega_tipo

FUNCTION genera_reporte()
#gr----------------------
--    DEFINE  vfecha_reporte DATE
    DEFINE  vlote          SMALLINT
    DEFINE  vnumero        SMALLINT
    DEFINE  vfolio         INTEGER
    DEFINE  vcopia         CHAR(200)
    DEFINE  ejecuta        CHAR(200)
    DEFINE  vnombre        CHAR(20)
    DEFINE  cont           SMALLINT
    DEFINE  cont1          SMALLINT
    DEFINE  xfolio         INTEGER 
    DEFINE reg_3  RECORD
        fecha_emision     DATE         ,
        fecha_valor       DATE         ,
        identificador     SMALLINT     ,
        transaccion_cod   INTEGER      ,
        siefore           SMALLINT     ,
        importe           DECIMAL(15,2),
        proceso_cod       CHAR(05)     
--        folio             INTEGER      
    END RECORD
    
    DEFINE reg_4 RECORD
        proceso_cod       CHAR(05)     ,
        transaccion_cod   INTEGER      ,
        tipo              CHAR(1)      ,
        identificador     SMALLINT     ,
        descripcion       CHAR(25)     ,
        cuenta            CHAR(09)     ,
        analisis_cod      CHAR(06)     ,
        importe           DECIMAL(15,2),
        id_cuenta         INTEGER      ,
        sumarizadora      CHAR(3)
    END RECORD

    DEFINE vproc12   CHAR(5) 
    DEFINE vtran12   INTEGER 
    DEFINE vcuenta12 CHAR(9) 
    DEFINE vtipo12   CHAR(1) 

    DEFINE vnum_max          SMALLINT
    DEFINE vnum_max_txt      CHAR(5) 
    DEFINE vfecha_gen        DATETIME HOUR TO SECOND 
    DEFINE mod_perm          CHAR(70)
    DEFINE mod_perm2         CHAR(70)
    DEFINE nombre_archivo    CHAR(30)
    DEFINE vcuenta           CHAR(09) 
    DEFINE vban_salida1      SMALLINT 
    DEFINE vban_salida2      SMALLINT 
    DEFINE vban_salida3      SMALLINT 
    DEFINE vban_salida4      SMALLINT 
    DEFINE vban_salida5      SMALLINT 
    DEFINE vban_fechavalor   SMALLINT 
    DEFINE vfe_emision       DATE     
    DEFINE vfe_valor         DATE     

    DEFINE vimporte          DECIMAL(15,2)
    DEFINE imp_tot_tras_sie1 DECIMAL(15,2)
    DEFINE imp_tot_tras_sie2 DECIMAL(15,2)
    DEFINE imp_tot_tras_sie3 DECIMAL(15,2)
    DEFINE imp_tot_tras_sie4 DECIMAL(15,2)
    DEFINE imp_tot_tras_sie5 DECIMAL(15,2)

    OPEN WINDOW ventana_2 AT 5,2 WITH FORM "CONM0013" ATTRIBUTE(BORDER)
    DISPLAY "          [ Esc ] Iniciar                            [ Ctrl-C ] Salir          " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "CONM001         GENERA ARCHIVO DE REGISTRO CONTABLE                            " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    LET vban_salida1 = 0
    LET vban_salida2 = 0
    LET vban_salida3 = 0
    LET vban_salida4 = 0
    LET vban_salida5 = 0
    LET ban_cumple   = 0
    INPUT vfecha_reporte FROM FORMONLY.vfecha_reporte

      AFTER FIELD vfecha_reporte
        IF vfecha_reporte IS NULL OR vfecha_reporte = " " THEN
           ERROR " Digite correctamente la Fecha "
           NEXT FIELD vfecha_reporte
        END IF

        SELECT "X"
        FROM   con_transaccion
        WHERE  fecha_emision  = vfecha_reporte
        AND    estado         = 20
        GROUP BY 1

        IF STATUS = NOTFOUND THEN
             ERROR "No hay archivo por generar"
             SLEEP 3
             EXIT PROGRAM
        ELSE 
             ERROR "GENERANDO REPORTE..."
        END IF
        SELECT MAX(lotes_correlativo) + 1
        INTO   vlote
        FROM   tab_lote
        WHERE  lotes_cod   = 15
        AND    lotes_fecha = HOY
    
        IF vlote IS NULL THEN
            LET vlote = 1
        END IF

        INSERT INTO tab_lote VALUES(HOY,15,"CONTABILIDAD",vlote,1)

        SELECT * 
        INTO   g_paramgrales.*
        FROM   seg_modulo
        WHERE  modulo_cod = "con"

        SELECT MAX(consecutivo)
        INTO   vnum_max
        FROM safre_tmp:nombre_archivo

        IF vnum_max IS NULL OR 
           vnum_max = "" THEN
            LET vnum_max = 0
        END IF

        LET vnum_max     = vnum_max + 1
        LET vnum_max_txt = vnum_max

        LET G_TRADUCTOR = g_paramgrales.ruta_envio CLIPPED,"/" CLIPPED,
                          "SALF_AFO_" CLIPPED,
                          HOY USING "YYMMDD" CLIPPED,"_",vnum_max_txt CLIPPED,
                          ".txt" CLIPPED
        LET nombre_archivo = "SALF_AFO_" CLIPPED,
                          HOY USING "YYMMDD" CLIPPED,"_",vnum_max_txt CLIPPED

        LET G_LISTA     = g_paramgrales.ruta_envio CLIPPED,"/" CLIPPED,"In"
                          CLIPPED, HOY using "YYMMDD" CLIPPED,"_",vnum_max_txt CLIPPED,
                          ".txt" CLIPPED

        LET vnombre = "In" CLIPPED, HOY using "YYMMDD" CLIPPED,"_",vnum_max_txt CLIPPED,
                      ".txt" CLIPPED

        CALL neteo_trans_sie()

        DECLARE cur_4 CURSOR FOR
        SELECT  a.proceso_cod,
                a.sumarizadora,
                a.cuenta,
                a.tipo,
                a.identificador,
                a.analisis_cod,
                b.fecha_emision,
                b.fecha_valor,
                0,
                sum(b.importe)
        FROM con_traductor a, con_transaccion b
        WHERE b.fecha_emision   = vfecha_reporte
        AND b.estado            = 20
        AND a.proceso_cod       = b.proceso_cod
        AND a.transaccion_cod   = b.transaccion_cod
        AND a.siefore           = b.siefore
        AND a.identificador     = b.identificador
        GROUP BY 1,2,3,4,5,6,7,8,9
        ORDER BY a.proceso_cod, a.sumarizadora
        START REPORT listado_3 TO G_LISTA
        START REPORT listado_4 TO G_TRADUCTOR
        LET cont     = 0
        LET xfolio   = 0
        LET vnumero  = 0
        LET contador = 0
        FOREACH cur_4 INTO reg_3.proceso_cod, reg_4.sumarizadora,
                           reg_4.cuenta, --reg_3.siefore,
                           reg_4.tipo, reg_3.identificador,
                           reg_4.analisis_cod,
                           reg_3.fecha_emision, reg_3.fecha_valor,
                           vnumero, reg_3.importe

           DECLARE cur_siefore CURSOR FOR
           SELECT  a.siefore
           FROM    con_traductor a
           WHERE   a.proceso_cod   = reg_3.proceso_cod
           AND     a.cuenta        = reg_4.cuenta
           AND     a.sumarizadora  = reg_4.sumarizadora
           AND     a.identificador = reg_3.identificador

           FOREACH cur_siefore INTO reg_3.siefore
           END FOREACH

           LET vproc12 = reg_3.proceso_cod
           LET vcuenta12 = reg_4.cuenta

              IF vproc12 = '00023' THEN

                IF reg_3.fecha_emision = reg_3.fecha_valor THEN
                    LET vban_fechavalor = 1
                ELSE
                    LET vban_fechavalor = 2
                END IF

                IF vban_fechavalor = 1 AND
                 ((vcuenta12 = "712827" AND   
                   reg_4.sumarizadora = 52) OR

                  (vcuenta12 = "722827" AND   
                   reg_4.sumarizadora = 51) OR

                  (vcuenta12 = "110201" AND
                   reg_4.sumarizadora = 54) OR

                  (vcuenta12 = "21010001" AND
                   reg_4.sumarizadora = 53) OR

                  (vcuenta12 = "712827" AND   
                   reg_4.sumarizadora = 52) OR

                  (vcuenta12 = "722827" AND   
                   reg_4.sumarizadora = 51) OR

                  (vcuenta12 = "110201" AND
                   reg_4.sumarizadora = 54) OR

                  (vcuenta12 = "21010001" AND
                   reg_4.sumarizadora = 53)) THEN

                   LET reg_3.importe = 0
                END IF

                IF vban_fechavalor = 2 THEN
                   IF (vcuenta12 = "712827" AND   
                       reg_4.sumarizadora = 41) OR

                      (vcuenta12 = "722827" AND   
                       reg_4.sumarizadora = 42) OR

                      (vcuenta12 = "110201" AND
                       reg_4.sumarizadora = 43) OR

                      (vcuenta12 = "21010001" AND
                       reg_4.sumarizadora = 44) THEN 

                       LET reg_3.importe = 0
                   END IF
                END IF
             END IF
---< erm 08 Nov 2005

---------------------->erm neteo 14 Junio 2006
      #proceso 00009 receptora menor
         IF reg_3.proceso_cod  = '00009' AND
            ban_neteo = 1 THEN             ---1 = existe cedente y receptora
            --IF vtot_09 < vtot_10     THEN
            IF vtotal_total_09 < vtotal_total_10     THEN 
               IF (reg_4.cuenta = "722827" AND            
                   reg_4.tipo   = 'C')   OR               
                  (reg_4.cuenta = "712827" AND            
                   reg_4.tipo   = 'A')   OR               
                   reg_4.cuenta = "110201" OR             
                   reg_4.cuenta = "21010001" OR           
                   reg_4.cuenta = "123210" OR             
                   reg_4.cuenta = "123220" THEN           
                  LET reg_3.importe = 0
               END IF
            ELSE

               --->erm 22 Ago 2006
               IF (reg_4.cuenta = "722827" AND  
                   reg_4.tipo   = 'C')    THEN    --AND
                   LET reg_3.importe = vtotal_total_09 - vtotal_total_10
               END IF

               IF (reg_4.cuenta = "712827" AND                    ---antes 713401
                   reg_4.tipo   = 'A')   THEN     ---AND
                   LET reg_3.importe = vtotal_total_09 - vtotal_total_10
               END IF

               IF (reg_4.cuenta = "110201" AND
                   reg_4.analisis_cod <> '1037') THEN

                   LET reg_3.importe = vtotal_total_09 - vtotal_total_10
               END IF

               IF (reg_4.cuenta = "21010001")  THEN
                   LET reg_3.importe = vtotal_total_09 - vtotal_total_10
               END IF
            END IF
         END IF
      #fracciones
         IF reg_3.proceso_cod  = '00009' AND
            ban_neteo = 1 THEN
            IF vtotal_total_09 < vtotal_total_10     THEN
               IF (reg_4.cuenta = "710710" AND 
                   reg_4.tipo   = 'C')   OR    
                  (reg_4.cuenta = "710720" AND 
                   reg_4.tipo   = 'C')   OR    
                  (reg_4.cuenta = "720710" AND 
                   reg_4.tipo   = 'A')   OR    
                  (reg_4.cuenta = "720720" AND 
                   reg_4.tipo   = 'A')   OR    
                   reg_4.cuenta = "123210" OR  
                   reg_4.cuenta = "123220" THEN
                  LET reg_3.importe = 0
               END IF

         END IF
               IF reg_4.cuenta = "123220" OR   
                 (reg_4.cuenta = "110201" AND
                  reg_4.sumarizadora = '027') THEN
                  LET reg_3.importe = vfracc_tot09_2_peso 
               END IF
               IF reg_4.cuenta = "123210" OR              
                 (reg_4.cuenta = "110201" AND
                  reg_4.sumarizadora = '026') THEN
                  LET reg_3.importe = vfracc_tot09_1_peso  
               END IF
--->5 sie
               IF reg_4.cuenta = "123230" OR      
                 (reg_4.cuenta = "110201" AND
                  reg_4.sumarizadora = '028') THEN
                  LET reg_3.importe = vfracc_tot09_3_peso  
               END IF
               IF reg_4.cuenta = "123240" OR     
                 (reg_4.cuenta = "110201" AND
                  reg_4.sumarizadora = '029') THEN
                  LET reg_3.importe = vfracc_tot09_4_peso  
               END IF
               IF reg_4.cuenta = "123250" OR       
                 (reg_4.cuenta = "110201" AND
                  reg_4.sumarizadora = '030') THEN
                  LET reg_3.importe = vfracc_tot09_5_peso  
               END IF
---<5 sie
               IF reg_4.cuenta = "710720" OR     
                  reg_4.cuenta = "720720" THEN   
                  LET reg_3.importe = vneto2   
               END IF
               IF reg_4.cuenta = "710710" OR      
                  reg_4.cuenta = "720710" THEN    
                  LET reg_3.importe = vneto1  
               END IF
--->5 sie
               IF reg_4.cuenta = "710730" OR       
                  reg_4.cuenta = "720730" THEN     
                  LET reg_3.importe = vneto3    
               END IF
               IF reg_4.cuenta = "710740" OR       
                  reg_4.cuenta = "720740" THEN     
                  LET reg_3.importe = vneto4   
               END IF
               IF reg_4.cuenta = "710750" OR      
                  reg_4.cuenta = "720750" THEN    
                  LET reg_3.importe = vneto5 
               END IF
---<5 sie
            END IF
         --END IF
      #prceso 00010 cedente menor
         IF reg_3.proceso_cod  = '00010' AND
            ban_neteo = 1 THEN              ---1 = existe cedente y receptora
            --IF vtot_10 < vtot_09     THEN
            IF vtotal_total_10 < vtotal_total_09     THEN 
               IF (reg_4.cuenta = "712827" AND            
                   reg_4.tipo   = 'C')   OR               
                  (reg_4.cuenta = "722827" AND            
                   reg_4.tipo   = 'A')   OR               
                  (reg_4.cuenta = "110201" OR             
                   reg_4.cuenta = "123220" OR             
                   reg_4.cuenta = "123210" OR             
                   reg_4.cuenta = "21010001") THEN        
                  LET reg_3.importe = 0
               END IF
            ELSE

               --->erm 22 Ago 2006
               IF (reg_4.cuenta = "712827" AND     
                   reg_4.tipo   = 'C')   THEN   
                   LET reg_3.importe = vtotal_total_10 - vtotal_total_09
               END IF

               IF (reg_4.cuenta = "722827" AND    
                   reg_4.tipo   = 'A')   THEN    
                   LET reg_3.importe = vtotal_total_10 - vtotal_total_09
               END IF

               IF (reg_4.cuenta = "110201" AND
                   reg_4.analisis_cod <> '1037') THEN   
                   LET reg_3.importe = vtotal_total_10 - vtotal_total_09
               END IF

               IF (reg_4.cuenta = "21010001")  THEN

                   LET reg_3.importe = vtotal_total_10 - vtotal_total_09
               END IF

            END IF
         END IF
      #fracciones
         IF reg_3.proceso_cod  = '00010' AND
            ban_neteo = 1 THEN
            --IF vtot_10 < vtot_09     THEN
            IF vtotal_total_10 < vtotal_total_09     THEN
               IF (reg_4.cuenta = "720720" AND    
                   reg_4.tipo   = 'C')   OR       
                  (reg_4.cuenta = "720710" AND    
                   reg_4.tipo   = 'C')   OR       
                  (reg_4.cuenta = "710720" AND    
                   reg_4.tipo   = 'A')   OR       
                  (reg_4.cuenta = "710710" AND    
                   reg_4.tipo   = 'A')   OR       
                   reg_4.cuenta = "123220" OR     
                   reg_4.cuenta = "123210" THEN   
                  LET reg_3.importe = 0         
               END IF                           
            --ELSE                              
           END IF                               
               IF reg_4.cuenta = "123220" OR        
                 (reg_4.cuenta = "110201" AND   
                  reg_4.sumarizadora = '112') THEN

                  LET reg_3.importe = vfracc_tot10_2_peso 
               END IF                           
               IF reg_4.cuenta = "123210" OR    
                 (reg_4.cuenta = "110201" AND
                  reg_4.sumarizadora = '111') THEN

                  LET reg_3.importe = vfracc_tot10_1_peso  
               END IF
--->5 sie
               IF reg_4.cuenta = "123230" OR     
                 (reg_4.cuenta = "110201" AND
                  reg_4.sumarizadora = '113') THEN
                   LET reg_3.importe = vfracc_tot10_3_peso   
               END IF
               IF reg_4.cuenta = "123240" OR          
                 (reg_4.cuenta = "110201" AND
                  reg_4.sumarizadora = '114') THEN

                  LET reg_3.importe = vfracc_tot10_4_peso   
               END IF
               IF reg_4.cuenta = "123250" OR      
                 (reg_4.cuenta = "110201" AND
                  reg_4.sumarizadora = '115') THEN
                  --LET reg_3.importe = vneto_fracc_peso_1
                  LET reg_3.importe = vfracc_tot10_5_peso   
               END IF
---<5 sie
               IF reg_4.cuenta = "710720" OR   
                  reg_4.cuenta = "720720" THEN 
                  LET reg_3.importe = vfracc_tot10_2_acc
               END IF
               IF reg_4.cuenta = "710710" OR      
                  reg_4.cuenta = "720710" THEN    
                  LET reg_3.importe = vfracc_tot10_1_acc
               END IF
--->5 sie
               IF reg_4.cuenta = "710730" OR  
                  reg_4.cuenta = "720730" THEN
                  LET reg_3.importe = vfracc_tot10_3_acc
               END IF
               IF reg_4.cuenta = "710740" OR   
                  reg_4.cuenta = "720740" THEN 
                  LET reg_3.importe = vfracc_tot10_4_acc
               END IF
               IF reg_4.cuenta = "710750" OR   
                  reg_4.cuenta = "720750" THEN 
                  LET reg_3.importe = vfracc_tot10_5_acc
               END IF
---<5 sie
            END IF
         --END IF
--------------------------------------------------<

------------------------>erm 13 Mayo 2008 transferencia entre siefores
         IF reg_3.proceso_cod = '00039' THEN 
--->neteo de cuentas de detalle 11 Sep 2008
#siefore 1 cuenta 721401 y contracuenta 711401 (retiro 97)
            IF ban21315_sie1 <> 0 THEN
               IF ban21315_sie1 = 1 THEN
                  IF (reg_4.cuenta = 721401 AND
                      reg_4.sumarizadora = '001') THEN
                        LET reg_3.importe = vimporte_21315_sie1
                  END IF
                  IF (reg_4.cuenta = 711401 AND
                      reg_4.sumarizadora = '032') THEN
                        LET reg_3.importe = vimporte_21315_sie1
                  END IF
                  IF (reg_4.cuenta = 711401 AND
                      reg_4.sumarizadora = '087') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721401 AND
                      reg_4.sumarizadora = '118') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#entrada mayor a salida
#siefore 1 cuenta 711401 y contracuenta 721401 (retiro 97)
               IF ban21315_sie1 = 2 THEN
                  IF (reg_4.cuenta = 721401 AND
                      reg_4.sumarizadora = '001') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711401 AND
                      reg_4.sumarizadora = '032') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711401 AND
                      reg_4.sumarizadora = '087') THEN
                        LET reg_3.importe = vimporte_21315_sie1
                  END IF
                  IF (reg_4.cuenta = 721401 AND
                      reg_4.sumarizadora = '118') THEN
                        LET reg_3.importe = vimporte_21315_sie1
                  END IF
               END IF
            END IF


#siefore 2 cuenta 721413 y contracuenta 7114013 (Retiro 97)
            IF ban21315_sie2 <> 0 THEN
               IF ban21315_sie2 = 1 THEN
                  IF (reg_4.cuenta = 721413 AND
                      reg_4.sumarizadora = '008') THEN
                        LET reg_3.importe = vimporte_21315_sie2
                  END IF
                  IF (reg_4.cuenta = 711413 AND
                      reg_4.sumarizadora = '039') THEN
                        LET reg_3.importe = vimporte_21315_sie2
                  END IF
                  IF (reg_4.cuenta = 711413 AND
                      reg_4.sumarizadora = '094') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721413 AND
                      reg_4.sumarizadora = '125') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 2 cuenta 711413 y contracuenta 7214013 (Retiro 97)
               IF ban21315_sie2 = 2 THEN
                  IF (reg_4.cuenta = 721413 AND
                      reg_4.sumarizadora = '008') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711413 AND
                      reg_4.sumarizadora = '039') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711413 AND
                      reg_4.sumarizadora = '094') THEN
                        LET reg_3.importe = vimporte_21315_sie2
                  END IF
                  IF (reg_4.cuenta = 721413 AND
                      reg_4.sumarizadora = '125') THEN
                        LET reg_3.importe = vimporte_21315_sie2
                  END IF
               END IF
            END IF

#siefore 3 cuenta 721425 y contracuenta 711425 (Retiro 97)
            IF ban21315_sie3 <> 0 THEN
               IF ban21315_sie3 = 1 THEN
                  IF (reg_4.cuenta = 721425 AND
                      reg_4.sumarizadora = '014') THEN
                        LET reg_3.importe = vimporte_21315_sie3
                  END IF
                  IF (reg_4.cuenta = 711425 AND
                      reg_4.sumarizadora = '045') THEN
                        LET reg_3.importe = vimporte_21315_sie3
                  END IF
                  IF (reg_4.cuenta = 711425 AND
                      reg_4.sumarizadora = '100') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721425 AND
                      reg_4.sumarizadora = '131') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 3 cuenta 711425 y contracuenta 721425 (Retiro 97)
               IF ban21315_sie3 = 2 THEN
                  IF (reg_4.cuenta = 721425 AND
                      reg_4.sumarizadora = '014') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711425 AND
                      reg_4.sumarizadora = '045') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711425 AND
                      reg_4.sumarizadora = '100') THEN
                        LET reg_3.importe = vimporte_21315_sie3
                  END IF
                  IF (reg_4.cuenta = 721425 AND
                      reg_4.sumarizadora = '131') THEN
                        LET reg_3.importe = vimporte_21315_sie3
                  END IF
               END IF
            END IF

#siefore 4 cuenta 721437 y contracuenta 711437 (Retiro 97)
            IF ban21315_sie4 <> 0 THEN
               IF ban21315_sie4 = 1 THEN
                  IF (reg_4.cuenta = 721437 AND
                      reg_4.sumarizadora = '020') THEN
                        LET reg_3.importe = vimporte_21315_sie4
                  END IF
                  IF (reg_4.cuenta = 711437 AND
                      reg_4.sumarizadora = '051') THEN
                        LET reg_3.importe = vimporte_21315_sie4
                  END IF
                  IF (reg_4.cuenta = 711437 AND
                      reg_4.sumarizadora = '106') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721437 AND
                      reg_4.sumarizadora = '137') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 4 cuenta 711437 y contracuenta 721437 (Retiro 97)
               IF ban21315_sie4 = 2 THEN
                  IF (reg_4.cuenta = 721437 AND
                      reg_4.sumarizadora = '020') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711437 AND
                      reg_4.sumarizadora = '051') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711437 AND
                      reg_4.sumarizadora = '106') THEN
                        LET reg_3.importe = vimporte_21315_sie4
                  END IF
                  IF (reg_4.cuenta = 721437 AND
                      reg_4.sumarizadora = '137') THEN
                        LET reg_3.importe = vimporte_21315_sie4
                  END IF
               END IF
            END IF

#siefore 5 cuenta 721449 y contracuenta 711449 (Retiro 97)
            IF ban21315_sie5 <> 0 THEN
               IF ban21315_sie5 = 1 THEN
                  IF (reg_4.cuenta = 721449 AND
                      reg_4.sumarizadora = '026') THEN
                        LET reg_3.importe = vimporte_21315_sie5
                  END IF
                  IF (reg_4.cuenta = 711449 AND
                      reg_4.sumarizadora = '057') THEN
                        LET reg_3.importe = vimporte_21315_sie5
                  END IF
                  IF (reg_4.cuenta = 711449 AND
                      reg_4.sumarizadora = '112') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721449 AND
                      reg_4.sumarizadora = '143') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 5 cuenta 711449 y contracuenta 721449 (Retiro 97)
               IF ban21315_sie5 = 2 THEN
                  IF (reg_4.cuenta = 721449 AND
                      reg_4.sumarizadora = '026') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711449 AND
                      reg_4.sumarizadora = '057') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711449 AND
                      reg_4.sumarizadora = '112') THEN
                        LET reg_3.importe = vimporte_21315_sie5
                  END IF
                  IF (reg_4.cuenta = 721449 AND
                      reg_4.sumarizadora = '143') THEN
                        LET reg_3.importe = vimporte_21315_sie5
                  END IF
               END IF
            END IF

#siefore 1 cuenta 721402 y contracuenta 711402 (cv,estatal,especial)
            IF ban22332_sie1 <> 0 THEN
               IF ban22332_sie1 = 1 THEN
                  IF (reg_4.cuenta = 721402 AND
                      reg_4.sumarizadora = '002') THEN
                        LET reg_3.importe = vimporte_22332_sie1
                  END IF
                  IF (reg_4.cuenta = 711402 AND
                      reg_4.sumarizadora = '033') THEN
                        LET reg_3.importe = vimporte_22332_sie1
                  END IF
                  IF (reg_4.cuenta = 711402 AND
                      reg_4.sumarizadora = '088') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721402 AND
                      reg_4.sumarizadora = '119') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 1 cuenta 711402 y contracuenta 721402 (cv,estatal,especial)
               IF ban22332_sie1 = 2 THEN
                  IF (reg_4.cuenta = 721402 AND
                      reg_4.sumarizadora = '002') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711402 AND
                      reg_4.sumarizadora = '033') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711402 AND
                      reg_4.sumarizadora = '088') THEN
                        LET reg_3.importe = vimporte_22332_sie1
                  END IF
                  IF (reg_4.cuenta = 721402 AND
                      reg_4.sumarizadora = '119') THEN
                        LET reg_3.importe = vimporte_22332_sie1
                  END IF
               END IF
            END IF

#siefore 2 cuenta 721414 y contracuenta 7114014 (cv,estatal,especial)
            IF ban22332_sie2 <> 0 THEN
               IF ban22332_sie2 = 1 THEN
                  IF (reg_4.cuenta = 721414 AND
                      reg_4.sumarizadora = '009') THEN
                        LET reg_3.importe = vimporte_22332_sie2
                  END IF
                  IF (reg_4.cuenta = 711414 AND
                      reg_4.sumarizadora = '040') THEN
                        LET reg_3.importe = vimporte_22332_sie2
                  END IF
                  IF (reg_4.cuenta = 711414 AND
                      reg_4.sumarizadora = '095') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721414 AND
                      reg_4.sumarizadora = '126') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 2 cuenta 711414 y contracuenta 7214014 (cv,estatal,especial)
               IF ban22332_sie2 = 2 THEN
                  IF (reg_4.cuenta = 721414 AND
                      reg_4.sumarizadora = '009') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711414 AND
                      reg_4.sumarizadora = '040') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711414 AND
                      reg_4.sumarizadora = '095') THEN
                        LET reg_3.importe = vimporte_22332_sie2
                  END IF
                  IF (reg_4.cuenta = 721414 AND
                      reg_4.sumarizadora = '126') THEN
                        LET reg_3.importe = vimporte_22332_sie2
                  END IF
               END IF
            END IF

#siefore 3 cuenta 721426 y contracuenta 711426 (cv,estatal,especial)
            IF ban22332_sie3 <> 0 THEN
               IF ban22332_sie3 = 1 THEN
                  IF (reg_4.cuenta = 721426 AND
                      reg_4.sumarizadora = '015') THEN
                        LET reg_3.importe = vimporte_22332_sie3
                  END IF
                  IF (reg_4.cuenta = 711426 AND
                      reg_4.sumarizadora = '046') THEN
                        LET reg_3.importe = vimporte_22332_sie3
                  END IF
                  IF (reg_4.cuenta = 711426 AND
                      reg_4.sumarizadora = '101') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721426 AND
                      reg_4.sumarizadora = '132') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 3 cuenta 711426 y contracuenta 721426 (cv,estatal,especial)
               IF ban22332_sie3 = 2 THEN
                  IF (reg_4.cuenta = 721426 AND
                      reg_4.sumarizadora = '015') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711426 AND
                      reg_4.sumarizadora = '046') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711426 AND
                      reg_4.sumarizadora = '101') THEN
                        LET reg_3.importe = vimporte_22332_sie3
                  END IF
                  IF (reg_4.cuenta = 721426 AND
                      reg_4.sumarizadora = '132') THEN
                        LET reg_3.importe = vimporte_22332_sie3
                  END IF
               END IF
            END IF

#siefore 4 cuenta 721438 y contracuenta 711438 (cv,estatal,especial)
            IF ban22332_sie4 <> 0 THEN
               IF ban22332_sie4 = 1 THEN
                  IF (reg_4.cuenta = 721438 AND
                      reg_4.sumarizadora = '021') THEN
                        LET reg_3.importe = vimporte_22332_sie4
                  END IF
                  IF (reg_4.cuenta = 711438 AND
                      reg_4.sumarizadora = '052') THEN
                        LET reg_3.importe = vimporte_22332_sie4
                  END IF
                  IF (reg_4.cuenta = 711438 AND
                      reg_4.sumarizadora = '107') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721438 AND
                      reg_4.sumarizadora = '138') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 4 cuenta 711438 y contracuenta 721438 (cv,estatal,especial)
               IF ban22332_sie4 = 2 THEN
                  IF (reg_4.cuenta = 721438 AND
                      reg_4.sumarizadora = '021') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711438 AND
                      reg_4.sumarizadora = '052') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711438 AND
                      reg_4.sumarizadora = '107') THEN
                        LET reg_3.importe = vimporte_22332_sie4
                  END IF
                  IF (reg_4.cuenta = 721438 AND
                      reg_4.sumarizadora = '138') THEN
                        LET reg_3.importe = vimporte_22332_sie4
                  END IF
               END IF
            END IF

#siefore 5 cuenta 721450 y contracuenta 711450 (cv,estatal,especial)
            IF ban22332_sie5 <> 0 THEN
               IF ban22332_sie5 = 1 THEN
                  IF (reg_4.cuenta = 721450 AND
                      reg_4.sumarizadora = '027') THEN
                        LET reg_3.importe = vimporte_22332_sie5
                  END IF
                  IF (reg_4.cuenta = 711450 AND
                      reg_4.sumarizadora = '058') THEN
                        LET reg_3.importe = vimporte_22332_sie5
                  END IF
                  IF (reg_4.cuenta = 711450 AND
                      reg_4.sumarizadora = '113') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721450 AND
                      reg_4.sumarizadora = '144') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 5 cuenta 711450 y contracuenta 721450 (cv,estatal,especial)
               IF ban22332_sie5 = 2 THEN
                  IF (reg_4.cuenta = 721450 AND
                      reg_4.sumarizadora = '027') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711450 AND
                      reg_4.sumarizadora = '058') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711450 AND
                      reg_4.sumarizadora = '113') THEN
                        LET reg_3.importe = vimporte_22332_sie5
                  END IF
                  IF (reg_4.cuenta = 721450 AND
                      reg_4.sumarizadora = '144') THEN
                        LET reg_3.importe = vimporte_22332_sie5
                  END IF
               END IF
            END IF

#siefore 1 cuenta 721403 y contracuenta 711403 (cs)
            IF ban23351_sie1 <> 0 THEN
               IF ban23351_sie1 = 1 THEN
                  IF (reg_4.cuenta = 721403 AND
                      reg_4.sumarizadora = '003') THEN
                        LET reg_3.importe = vimporte_23351_sie1
                  END IF
                  IF (reg_4.cuenta = 711403 AND
                      reg_4.sumarizadora = '034') THEN
                        LET reg_3.importe = vimporte_23351_sie1
                  END IF
                  IF (reg_4.cuenta = 711403 AND
                      reg_4.sumarizadora = '089') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721403 AND
                      reg_4.sumarizadora = '120') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 1 cuenta 711403 y contracuenta 721403 (cs)
               IF ban23351_sie1 = 2 THEN
                  IF (reg_4.cuenta = 721403 AND
                      reg_4.sumarizadora = '003') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711403 AND
                      reg_4.sumarizadora = '034') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711403 AND
                      reg_4.sumarizadora = '089') THEN
                        LET reg_3.importe = vimporte_23351_sie1
                  END IF
                  IF (reg_4.cuenta = 721403 AND
                      reg_4.sumarizadora = '120') THEN
                        LET reg_3.importe = vimporte_23351_sie1
                  END IF
               END IF
            END IF

#siefore 2 cuenta 721415 y contracuenta 7114015 (cs)
            IF ban23351_sie2 <> 0 THEN
               IF ban23351_sie2 = 1 THEN
                  IF (reg_4.cuenta = 721415 AND
                      reg_4.sumarizadora = '010') THEN
                        LET reg_3.importe = vimporte_23351_sie2
                  END IF
                  IF (reg_4.cuenta = 711415 AND
                      reg_4.sumarizadora = '041') THEN
                        LET reg_3.importe = vimporte_23351_sie2
                  END IF
                  IF (reg_4.cuenta = 711415 AND
                      reg_4.sumarizadora = '096') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721415 AND
                      reg_4.sumarizadora = '127') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 2 cuenta 711415 y contracuenta 7214015 (cs)
               IF ban23351_sie2 = 2 THEN
                  IF (reg_4.cuenta = 721415 AND
                      reg_4.sumarizadora = '010') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711415 AND
                      reg_4.sumarizadora = '041') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711415 AND
                      reg_4.sumarizadora = '096') THEN
                        LET reg_3.importe = vimporte_23351_sie2
                  END IF
                  IF (reg_4.cuenta = 721415 AND
                      reg_4.sumarizadora = '127') THEN
                        LET reg_3.importe = vimporte_23351_sie2
                  END IF
               END IF
            END IF

#siefore 3 cuenta 721427 y contracuenta 711427 (cs)
            IF ban23351_sie3 <> 0 THEN
               IF ban23351_sie3 = 1 THEN
                  IF (reg_4.cuenta = 721427 AND
                      reg_4.sumarizadora = '016') THEN
                        LET reg_3.importe = vimporte_23351_sie3
                  END IF
                  IF (reg_4.cuenta = 711427 AND
                      reg_4.sumarizadora = '047') THEN
                        LET reg_3.importe = vimporte_23351_sie3
                  END IF
                  IF (reg_4.cuenta = 711427 AND
                      reg_4.sumarizadora = '102') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721427 AND
                      reg_4.sumarizadora = '133') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 3 cuenta 711427 y contracuenta 721427 (cs)
               IF ban23351_sie3 = 2 THEN
                  IF (reg_4.cuenta = 721427 AND
                      reg_4.sumarizadora = '016') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711427 AND
                      reg_4.sumarizadora = '047') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711427 AND
                      reg_4.sumarizadora = '102') THEN
                        LET reg_3.importe = vimporte_23351_sie3
                  END IF
                  IF (reg_4.cuenta = 721427 AND
                      reg_4.sumarizadora = '133') THEN
                        LET reg_3.importe = vimporte_23351_sie3
                  END IF
               END IF
            END IF

#siefore 4 cuenta 721439 y contracuenta 711439 (cs)
            IF ban23351_sie4 <> 0 THEN
               IF ban23351_sie4 = 1 THEN
                  IF (reg_4.cuenta = 721439 AND
                      reg_4.sumarizadora = '022') THEN
                        LET reg_3.importe = vimporte_23351_sie4
                  END IF
                  IF (reg_4.cuenta = 711439 AND
                      reg_4.sumarizadora = '053') THEN
                        LET reg_3.importe = vimporte_23351_sie4
                  END IF
                  IF (reg_4.cuenta = 711439 AND
                      reg_4.sumarizadora = '108') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721439 AND
                      reg_4.sumarizadora = '139') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 4 cuenta 711439 y contracuenta 721439 (cs)
               IF ban23351_sie4 = 2 THEN
                  IF (reg_4.cuenta = 721439 AND
                      reg_4.sumarizadora = '022') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711439 AND
                      reg_4.sumarizadora = '053') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711439 AND
                      reg_4.sumarizadora = '108') THEN
                        LET reg_3.importe = vimporte_23351_sie4
                  END IF
                  IF (reg_4.cuenta = 721439 AND
                      reg_4.sumarizadora = '139') THEN
                        LET reg_3.importe = vimporte_23351_sie4
                  END IF
               END IF
            END IF

#siefore 5 cuenta 721451 y contracuenta 711451 (cs)
            IF ban23351_sie5 <> 0 THEN
               IF ban23351_sie5 = 1 THEN
                  IF (reg_4.cuenta = 721451 AND
                      reg_4.sumarizadora = '028') THEN
                        LET reg_3.importe = vimporte_23351_sie5
                  END IF
                  IF (reg_4.cuenta = 711451 AND
                      reg_4.sumarizadora = '059') THEN
                        LET reg_3.importe = vimporte_23351_sie5
                  END IF
                  IF (reg_4.cuenta = 711451 AND
                      reg_4.sumarizadora = '114') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721451 AND
                      reg_4.sumarizadora = '145') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 5 cuenta 711451 y contracuenta 721451 (cs)
               IF ban23351_sie5 = 2 THEN
                  IF (reg_4.cuenta = 721451 AND
                      reg_4.sumarizadora = '028') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711451 AND
                      reg_4.sumarizadora = '059') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711451 AND
                      reg_4.sumarizadora = '114') THEN
                        LET reg_3.importe = vimporte_23351_sie5
                  END IF
                  IF (reg_4.cuenta = 721451 AND
                      reg_4.sumarizadora = '145') THEN
                        LET reg_3.importe = vimporte_23351_sie5
                  END IF
               END IF
            END IF

#siefore 1 cuenta 721404 y contracuenta 711404 (sar 92)
            IF ban28351_sie1 <> 0 THEN
               IF ban28351_sie1 = 1 THEN
                  IF (reg_4.cuenta = 721404 AND
                      reg_4.sumarizadora = '004') THEN
                        LET reg_3.importe = vimporte_28351_sie1
                  END IF
                  IF (reg_4.cuenta = 711404 AND
                      reg_4.sumarizadora = '035') THEN
                        LET reg_3.importe = vimporte_28351_sie1
                  END IF
                  IF (reg_4.cuenta = 711404 AND
                      reg_4.sumarizadora = '090') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721404 AND
                      reg_4.sumarizadora = '121') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 1 cuenta 711404 y contracuenta 721404 (sar 92)
               IF ban28351_sie1 = 2 THEN
                  IF (reg_4.cuenta = 721404 AND
                      reg_4.sumarizadora = '004') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711404 AND
                      reg_4.sumarizadora = '035') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711404 AND
                      reg_4.sumarizadora = '090') THEN
                        LET reg_3.importe = vimporte_28351_sie1
                  END IF
                  IF (reg_4.cuenta = 721404 AND
                      reg_4.sumarizadora = '121') THEN
                        LET reg_3.importe = vimporte_28351_sie1
                  END IF
               END IF
            END IF

#siefore 2 cuenta 721416 y contracuenta 711416 (sar 92)
            IF ban28351_sie2 <> 0 THEN
               IF ban28351_sie2 = 1 THEN
                  IF (reg_4.cuenta = 721416 AND
                      reg_4.sumarizadora = '011') THEN
                        LET reg_3.importe = vimporte_28351_sie2
                  END IF
                  IF (reg_4.cuenta = 711416 AND
                      reg_4.sumarizadora = '042') THEN
                        LET reg_3.importe = vimporte_28351_sie2
                  END IF
                  IF (reg_4.cuenta = 711416 AND
                      reg_4.sumarizadora = '097') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721416 AND
                      reg_4.sumarizadora = '128') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 2 cuenta 711416 y contracuenta 7214016 (sar 92)
               IF ban28351_sie2 = 2 THEN
                  IF (reg_4.cuenta = 721416 AND
                      reg_4.sumarizadora = '011') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711416 AND
                      reg_4.sumarizadora = '042') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711416 AND
                      reg_4.sumarizadora = '097') THEN
                        LET reg_3.importe = vimporte_28351_sie2
                  END IF
                  IF (reg_4.cuenta = 721416 AND
                      reg_4.sumarizadora = '128') THEN
                        LET reg_3.importe = vimporte_28351_sie2
                  END IF
               END IF
            END IF

#siefore 3 cuenta 721428 y contracuenta 711428 (sar 92)
            IF ban28351_sie3 <> 0 THEN
               IF ban28351_sie3 = 1 THEN
                  IF (reg_4.cuenta = 721428 AND
                      reg_4.sumarizadora = '017') THEN
                        LET reg_3.importe = vimporte_28351_sie3
                  END IF
                  IF (reg_4.cuenta = 711428 AND
                      reg_4.sumarizadora = '048') THEN
                        LET reg_3.importe = vimporte_28351_sie3
                  END IF
                  IF (reg_4.cuenta = 711428 AND
                      reg_4.sumarizadora = '103') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721428 AND
                      reg_4.sumarizadora = '134') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 3 cuenta 711428 y contracuenta 721428 (sar 92)
               IF ban28351_sie3 = 2 THEN
                  IF (reg_4.cuenta = 721428 AND
                      reg_4.sumarizadora = '017') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711428 AND
                      reg_4.sumarizadora = '048') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711428 AND
                      reg_4.sumarizadora = '103') THEN
                        LET reg_3.importe = vimporte_28351_sie3
                  END IF
                  IF (reg_4.cuenta = 721428 AND
                      reg_4.sumarizadora = '134') THEN
                        LET reg_3.importe = vimporte_28351_sie3
                  END IF
               END IF
            END IF

#siefore 4 cuenta 721440 y contracuenta 711440 (sar 92)
            IF ban28351_sie4 <> 0 THEN
               IF ban28351_sie4 = 1 THEN
                  IF (reg_4.cuenta = 721440 AND
                      reg_4.sumarizadora = '023') THEN
                        LET reg_3.importe = vimporte_28351_sie4
                  END IF
                  IF (reg_4.cuenta = 711440 AND
                      reg_4.sumarizadora = '054') THEN
                        LET reg_3.importe = vimporte_28351_sie4
                  END IF
                  IF (reg_4.cuenta = 711440 AND
                      reg_4.sumarizadora = '109') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721440 AND
                      reg_4.sumarizadora = '140') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 4 cuenta 711440 y contracuenta 721440 (sar 92)
               IF ban28351_sie4 = 2 THEN
                  IF (reg_4.cuenta = 721440 AND
                      reg_4.sumarizadora = '023') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711440 AND
                      reg_4.sumarizadora = '054') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711440 AND
                      reg_4.sumarizadora = '109') THEN
                        LET reg_3.importe = vimporte_28351_sie4
                  END IF
                  IF (reg_4.cuenta = 721440 AND
                      reg_4.sumarizadora = '140') THEN
                        LET reg_3.importe = vimporte_28351_sie4
                  END IF
               END IF
            END IF

#siefore 5 cuenta 721452 y contracuenta 711452 (sar 92)
            IF ban28351_sie5 <> 0 THEN
               IF ban28351_sie5 = 1 THEN
                  IF (reg_4.cuenta = 721452 AND
                      reg_4.sumarizadora = '029') THEN
                        LET reg_3.importe = vimporte_28351_sie5
                  END IF
                  IF (reg_4.cuenta = 711452 AND
                      reg_4.sumarizadora = '060') THEN
                        LET reg_3.importe = vimporte_28351_sie5
                  END IF
                  IF (reg_4.cuenta = 711452 AND
                      reg_4.sumarizadora = '115') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721452 AND
                      reg_4.sumarizadora = '147') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 5 cuenta 711452 y contracuenta 721452 (sar 92)
               IF ban28351_sie5 = 2 THEN
                  IF (reg_4.cuenta = 721452 AND
                      reg_4.sumarizadora = '029') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711452 AND
                      reg_4.sumarizadora = '060') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711452 AND
                      reg_4.sumarizadora = '115') THEN
                        LET reg_3.importe = vimporte_28351_sie5
                  END IF
                  IF (reg_4.cuenta = 721452 AND
                      reg_4.sumarizadora = '147') THEN
                        LET reg_3.importe = vimporte_28351_sie5
                  END IF
               END IF
            END IF

#>>>>>>
#siefore 1 cuenta 721409 y contracuenta 711409 (retiro issste)
            IF ban29124_sie1 <> 0 THEN
               IF ban29124_sie1 = 1 THEN
                  IF (reg_4.cuenta = 721409 AND
                      reg_4.sumarizadora = '006') THEN
                        LET reg_3.importe = vimporte_29124_sie1
                  END IF
                  IF (reg_4.cuenta = 711409 AND
                      reg_4.sumarizadora = '037') THEN
                        LET reg_3.importe = vimporte_29124_sie1
                  END IF
                  IF (reg_4.cuenta = 711409 AND
                      reg_4.sumarizadora = '092') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721409 AND
                      reg_4.sumarizadora = '123') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#entrada mayor a salida
#siefore 1 cuenta 711409 y contracuenta 721409 (retiro issste)
               IF ban29124_sie1 = 2 THEN
                  IF (reg_4.cuenta = 721409 AND
                      reg_4.sumarizadora = '006') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711409 AND
                      reg_4.sumarizadora = '037') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711409 AND
                      reg_4.sumarizadora = '092') THEN
                        LET reg_3.importe = vimporte_29124_sie1
                  END IF
                  IF (reg_4.cuenta = 721409 AND
                      reg_4.sumarizadora = '123') THEN
                        LET reg_3.importe = vimporte_29124_sie1
                  END IF
               END IF
            END IF

#siefore 2 cuenta 721421 y contracuenta 7114021 (Retiro issste)
            IF ban29124_sie2 <> 0 THEN
               IF ban29124_sie2 = 1 THEN
                  IF (reg_4.cuenta = 721421 AND
                      reg_4.sumarizadora = '012') THEN
                        LET reg_3.importe = vimporte_29124_sie2
                  END IF
                  IF (reg_4.cuenta = 711421 AND
                      reg_4.sumarizadora = '043') THEN
                        LET reg_3.importe = vimporte_29124_sie2
                  END IF
                  IF (reg_4.cuenta = 711421 AND
                      reg_4.sumarizadora = '098') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721421 AND
                      reg_4.sumarizadora = '129') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 2 cuenta 711421 y contracuenta 7214021 (Retiro issste)
               IF ban29124_sie2 = 2 THEN
                  IF (reg_4.cuenta = 721421 AND
                      reg_4.sumarizadora = '012') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711421 AND
                      reg_4.sumarizadora = '043') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711421 AND
                      reg_4.sumarizadora = '098') THEN
                        LET reg_3.importe = vimporte_29124_sie2
                  END IF
                  IF (reg_4.cuenta = 721421 AND
                      reg_4.sumarizadora = '129') THEN
                        LET reg_3.importe = vimporte_29124_sie2
                  END IF
               END IF
            END IF

#siefore 3 cuenta 721433 y contracuenta 711433 (Retiro issste)
            IF ban29124_sie3 <> 0 THEN
               IF ban29124_sie3 = 1 THEN
                  IF (reg_4.cuenta = 721433 AND
                      reg_4.sumarizadora = '018') THEN
                        LET reg_3.importe = vimporte_29124_sie3
                  END IF
                  IF (reg_4.cuenta = 711433 AND
                      reg_4.sumarizadora = '049') THEN
                        LET reg_3.importe = vimporte_29124_sie3
                  END IF
                  IF (reg_4.cuenta = 711433 AND
                      reg_4.sumarizadora = '104') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721433 AND
                      reg_4.sumarizadora = '135') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 3 cuenta 711433 y contracuenta 721433 (Retiro issste)
               IF ban29124_sie3 = 2 THEN
                  IF (reg_4.cuenta = 721433 AND
                      reg_4.sumarizadora = '018') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711433 AND
                      reg_4.sumarizadora = '049') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711433 AND
                      reg_4.sumarizadora = '104') THEN
                        LET reg_3.importe = vimporte_29124_sie3
                  END IF
                  IF (reg_4.cuenta = 721433 AND
                      reg_4.sumarizadora = '135') THEN
                        LET reg_3.importe = vimporte_29124_sie3
                  END IF
               END IF
            END IF

#siefore 4 cuenta 721445 y contracuenta 711445 (Retiro issste)
            IF ban29124_sie4 <> 0 THEN
               IF ban29124_sie4 = 1 THEN
                  IF (reg_4.cuenta = 721445 AND
                      reg_4.sumarizadora = '024') THEN
                        LET reg_3.importe = vimporte_29124_sie4
                  END IF
                  IF (reg_4.cuenta = 711445 AND
                      reg_4.sumarizadora = '055') THEN
                        LET reg_3.importe = vimporte_29124_sie4
                  END IF
                  IF (reg_4.cuenta = 711445 AND
                      reg_4.sumarizadora = '110') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721445 AND
                      reg_4.sumarizadora = '141') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 4 cuenta 711445 y contracuenta 721445 (Retiro 97)
               IF ban29124_sie4 = 2 THEN
                  IF (reg_4.cuenta = 721445 AND
                      reg_4.sumarizadora = '024') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711445 AND
                      reg_4.sumarizadora = '055') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711445 AND
                      reg_4.sumarizadora = '110') THEN
                        LET reg_3.importe = vimporte_29124_sie4
                  END IF
                  IF (reg_4.cuenta = 721445 AND
                      reg_4.sumarizadora = '141') THEN
                        LET reg_3.importe = vimporte_29124_sie4
                  END IF
               END IF
            END IF

#siefore 5 cuenta 721457 y contracuenta 711457 (Retiro issste)
            IF ban29124_sie5 <> 0 THEN
               IF ban29124_sie5 = 1 THEN
                  IF (reg_4.cuenta = 721457 AND
                      reg_4.sumarizadora = '030') THEN
                        LET reg_3.importe = vimporte_29124_sie5
                  END IF
                  IF (reg_4.cuenta = 711457 AND
                      reg_4.sumarizadora = '061') THEN
                        LET reg_3.importe = vimporte_29124_sie5
                  END IF
                  IF (reg_4.cuenta = 711457 AND
                      reg_4.sumarizadora = '116') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721457 AND
                      reg_4.sumarizadora = '147') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 5 cuenta 711457 y contracuenta 721457 (Retiro issste)
               IF ban29124_sie5 = 2 THEN
                  IF (reg_4.cuenta = 721457 AND
                      reg_4.sumarizadora = '030') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711457 AND
                      reg_4.sumarizadora = '061') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711457 AND
                      reg_4.sumarizadora = '116') THEN
                        LET reg_3.importe = vimporte_29124_sie5
                  END IF
                  IF (reg_4.cuenta = 721457 AND
                      reg_4.sumarizadora = '147') THEN
                        LET reg_3.importe = vimporte_29124_sie5
                  END IF
               END IF
            END IF


#siefore 1 cuenta 721410 y contracuenta 711410 (cv issste)
            IF ban29127_sie1 <> 0 THEN
               IF ban29127_sie1 = 1 THEN
                  IF (reg_4.cuenta = 721410 AND
                      reg_4.sumarizadora = '007') THEN
                        LET reg_3.importe = vimporte_29127_sie1
                  END IF
                  IF (reg_4.cuenta = 711410 AND
                      reg_4.sumarizadora = '038') THEN
                        LET reg_3.importe = vimporte_29127_sie1
                  END IF
                  IF (reg_4.cuenta = 711410 AND
                      reg_4.sumarizadora = '093') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721410 AND
                      reg_4.sumarizadora = '124') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#entrada mayor a salida
#siefore 1 cuenta 711410 y contracuenta 721410 (cv issste)
               IF ban29127_sie1 = 2 THEN
                  IF (reg_4.cuenta = 721410 AND
                      reg_4.sumarizadora = '007') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711410 AND
                      reg_4.sumarizadora = '038') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711410 AND
                      reg_4.sumarizadora = '093') THEN
                        LET reg_3.importe = vimporte_29127_sie1
                  END IF
                  IF (reg_4.cuenta = 721410 AND
                      reg_4.sumarizadora = '124') THEN
                        LET reg_3.importe = vimporte_29127_sie1
                  END IF
               END IF
            END IF

#siefore 2 cuenta 721422 y contracuenta 7114022 (cv issste)
            IF ban29127_sie2 <> 0 THEN
               IF ban29127_sie2 = 1 THEN
                  IF (reg_4.cuenta = 721422 AND
                      reg_4.sumarizadora = '013') THEN
                        LET reg_3.importe = vimporte_29127_sie2
                  END IF
                  IF (reg_4.cuenta = 711422 AND
                      reg_4.sumarizadora = '044') THEN
                        LET reg_3.importe = vimporte_29124_sie2
                  END IF
                  IF (reg_4.cuenta = 711422 AND
                      reg_4.sumarizadora = '099') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721422 AND
                      reg_4.sumarizadora = '130') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 2 cuenta 711422 y contracuenta 7214022 (cv     issste)
               IF ban29127_sie2 = 2 THEN
                  IF (reg_4.cuenta = 721422 AND
                      reg_4.sumarizadora = '013') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711422 AND
                      reg_4.sumarizadora = '044') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711422 AND
                      reg_4.sumarizadora = '099') THEN
                        LET reg_3.importe = vimporte_29127_sie2
                  END IF
                  IF (reg_4.cuenta = 721422 AND
                      reg_4.sumarizadora = '130') THEN
                        LET reg_3.importe = vimporte_29127_sie2
                  END IF
               END IF
            END IF

#siefore 3 cuenta 721434 y contracuenta 711434 (cv     issste)
            IF ban29127_sie3 <> 0 THEN
               IF ban29127_sie3 = 1 THEN
                  IF (reg_4.cuenta = 721434 AND
                      reg_4.sumarizadora = '019') THEN
                        LET reg_3.importe = vimporte_29127_sie3
                  END IF
                  IF (reg_4.cuenta = 711434 AND
                      reg_4.sumarizadora = '050') THEN
                        LET reg_3.importe = vimporte_29127_sie3
                  END IF
                  IF (reg_4.cuenta = 711434 AND
                      reg_4.sumarizadora = '105') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721434 AND
                      reg_4.sumarizadora = '136') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 3 cuenta 711434 y contracuenta 721434 (cv     issste)
               IF ban29127_sie3 = 2 THEN
                  IF (reg_4.cuenta = 721434 AND
                      reg_4.sumarizadora = '019') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711434 AND
                      reg_4.sumarizadora = '050') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711434 AND
                      reg_4.sumarizadora = '105') THEN
                        LET reg_3.importe = vimporte_29127_sie3
                  END IF
                  IF (reg_4.cuenta = 721434 AND
                      reg_4.sumarizadora = '136') THEN
                        LET reg_3.importe = vimporte_29127_sie3
                  END IF
               END IF
            END IF

#siefore 4 cuenta 721446 y contracuenta 711446 (cv     issste)
            IF ban29127_sie4 <> 0 THEN
               IF ban29127_sie4 = 1 THEN
                  IF (reg_4.cuenta = 721446 AND
                      reg_4.sumarizadora = '025') THEN
                        LET reg_3.importe = vimporte_29127_sie4
                  END IF
                  IF (reg_4.cuenta = 711446 AND
                      reg_4.sumarizadora = '056') THEN
                        LET reg_3.importe = vimporte_29127_sie4
                  END IF
                  IF (reg_4.cuenta = 711446 AND
                      reg_4.sumarizadora = '111') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721446 AND
                      reg_4.sumarizadora = '142') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 4 cuenta 711446 y contracuenta 721446 (cv     97)
               IF ban29127_sie4 = 2 THEN
                  IF (reg_4.cuenta = 721446 AND
                      reg_4.sumarizadora = '025') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711446 AND
                      reg_4.sumarizadora = '056') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711446 AND
                      reg_4.sumarizadora = '111') THEN
                        LET reg_3.importe = vimporte_29127_sie4
                  END IF
                  IF (reg_4.cuenta = 721446 AND
                      reg_4.sumarizadora = '142') THEN
                        LET reg_3.importe = vimporte_29127_sie4
                  END IF
               END IF
            END IF

#siefore 5 cuenta 721458 y contracuenta 711458 (cv     issste)
            IF ban29127_sie5 <> 0 THEN
               IF ban29127_sie5 = 1 THEN
                  IF (reg_4.cuenta = 721458 AND
                      reg_4.sumarizadora = '031') THEN
                        LET reg_3.importe = vimporte_29127_sie5
                  END IF
                  IF (reg_4.cuenta = 711458 AND
                      reg_4.sumarizadora = '062') THEN
                        LET reg_3.importe = vimporte_29127_sie5
                  END IF
                  IF (reg_4.cuenta = 711458 AND
                      reg_4.sumarizadora = '117') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721458 AND
                      reg_4.sumarizadora = '148') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 5 cuenta 711458 y contracuenta 721458 (Retiro issste)
               IF ban29127_sie5 = 2 THEN
                  IF (reg_4.cuenta = 721458 AND
                      reg_4.sumarizadora = '031') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711458 AND
                      reg_4.sumarizadora = '062') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711458 AND
                      reg_4.sumarizadora = '117') THEN
                        LET reg_3.importe = vimporte_29127_sie5
                  END IF
                  IF (reg_4.cuenta = 721458 AND
                      reg_4.sumarizadora = '148') THEN
                        LET reg_3.importe = vimporte_29127_sie5
                  END IF
               END IF
            END IF

#siefore 1 cuenta 721410 y contracuenta 711410 (cS issste)
            IF ban29128_sie1 <> 0 THEN
               IF ban29128_sie1 = 1 THEN
                  IF (reg_4.cuenta = 721410 AND
                      reg_4.sumarizadora = '007') THEN
                        LET reg_3.importe = vimporte_29128_sie1
                  END IF
                  IF (reg_4.cuenta = 711410 AND
                      reg_4.sumarizadora = '038') THEN
                        LET reg_3.importe = vimporte_29128_sie1
                  END IF
                  IF (reg_4.cuenta = 711410 AND
                      reg_4.sumarizadora = '093') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721410 AND
                      reg_4.sumarizadora = '124') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#entrada mayor a salida
#siefore 1 cuenta 711410 y contracuenta 721410 (cv issste)
               IF ban29128_sie1 = 2 THEN
                  IF (reg_4.cuenta = 721410 AND
                      reg_4.sumarizadora = '007') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711410 AND
                      reg_4.sumarizadora = '038') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711410 AND
                      reg_4.sumarizadora = '093') THEN
                        LET reg_3.importe = vimporte_29128_sie1
                  END IF
                  IF (reg_4.cuenta = 721410 AND
                      reg_4.sumarizadora = '124') THEN
                        LET reg_3.importe = vimporte_29128_sie1
                  END IF
               END IF
            END IF

#siefore 2 cuenta 721422 y contracuenta 7114022 (cv issste)
            IF ban29128_sie2 <> 0 THEN
               IF ban29128_sie2 = 1 THEN
                  IF (reg_4.cuenta = 721422 AND
                      reg_4.sumarizadora = '013') THEN
                        LET reg_3.importe = vimporte_29128_sie2
                  END IF
                  IF (reg_4.cuenta = 711422 AND
                      reg_4.sumarizadora = '044') THEN
                        LET reg_3.importe = vimporte_29124_sie2
                  END IF
                  IF (reg_4.cuenta = 711422 AND
                      reg_4.sumarizadora = '099') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721422 AND
                      reg_4.sumarizadora = '130') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 2 cuenta 711422 y contracuenta 7214022 (cv     issste)
               IF ban29128_sie2 = 2 THEN
                  IF (reg_4.cuenta = 721422 AND
                      reg_4.sumarizadora = '013') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711422 AND
                      reg_4.sumarizadora = '044') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711422 AND
                      reg_4.sumarizadora = '099') THEN
                        LET reg_3.importe = vimporte_29128_sie2
                  END IF
                  IF (reg_4.cuenta = 721422 AND
                      reg_4.sumarizadora = '130') THEN
                        LET reg_3.importe = vimporte_29128_sie2
                  END IF
               END IF
            END IF

#siefore 3 cuenta 721434 y contracuenta 711434 (cv     issste)
            IF ban29128_sie3 <> 0 THEN
               IF ban29128_sie3 = 1 THEN
                  IF (reg_4.cuenta = 721434 AND
                      reg_4.sumarizadora = '019') THEN
                        LET reg_3.importe = vimporte_29128_sie3
                  END IF
                  IF (reg_4.cuenta = 711434 AND
                      reg_4.sumarizadora = '050') THEN
                        LET reg_3.importe = vimporte_29128_sie3
                  END IF
                  IF (reg_4.cuenta = 711434 AND
                      reg_4.sumarizadora = '105') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721434 AND
                      reg_4.sumarizadora = '136') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 3 cuenta 711434 y contracuenta 721434 (cv     issste)
               IF ban29128_sie3 = 2 THEN
                  IF (reg_4.cuenta = 721434 AND
                      reg_4.sumarizadora = '019') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711434 AND
                      reg_4.sumarizadora = '050') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711434 AND
                      reg_4.sumarizadora = '105') THEN
                        LET reg_3.importe = vimporte_29128_sie3
                  END IF
                  IF (reg_4.cuenta = 721434 AND
                      reg_4.sumarizadora = '136') THEN
                        LET reg_3.importe = vimporte_29128_sie3
                  END IF
               END IF
            END IF

#siefore 4 cuenta 721446 y contracuenta 711446 (cv     issste)
            IF ban29128_sie4 <> 0 THEN
               IF ban29128_sie4 = 1 THEN
                  IF (reg_4.cuenta = 721446 AND
                      reg_4.sumarizadora = '025') THEN
                        LET reg_3.importe = vimporte_29128_sie4
                  END IF
                  IF (reg_4.cuenta = 711446 AND
                      reg_4.sumarizadora = '056') THEN
                        LET reg_3.importe = vimporte_29128_sie4
                  END IF
                  IF (reg_4.cuenta = 711446 AND
                      reg_4.sumarizadora = '111') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721446 AND
                      reg_4.sumarizadora = '142') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 4 cuenta 711446 y contracuenta 721446 (cv     97)
               IF ban29128_sie4 = 2 THEN
                  IF (reg_4.cuenta = 721446 AND
                      reg_4.sumarizadora = '025') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711446 AND
                      reg_4.sumarizadora = '056') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711446 AND
                      reg_4.sumarizadora = '111') THEN
                        LET reg_3.importe = vimporte_29128_sie4
                  END IF
                  IF (reg_4.cuenta = 721446 AND
                      reg_4.sumarizadora = '142') THEN
                        LET reg_3.importe = vimporte_29128_sie4
                  END IF
               END IF
            END IF

#siefore 5 cuenta 721458 y contracuenta 711458 (cv     issste)
            IF ban29128_sie5 <> 0 THEN
               IF ban29128_sie5 = 1 THEN
                  IF (reg_4.cuenta = 721458 AND
                      reg_4.sumarizadora = '031') THEN
                        LET reg_3.importe = vimporte_29128_sie5
                  END IF
                  IF (reg_4.cuenta = 711458 AND
                      reg_4.sumarizadora = '062') THEN
                        LET reg_3.importe = vimporte_29128_sie5
                  END IF
                  IF (reg_4.cuenta = 711458 AND
                      reg_4.sumarizadora = '117') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 721458 AND
                      reg_4.sumarizadora = '148') THEN
                        LET reg_3.importe = 0
                  END IF
               END IF

#siefore 5 cuenta 711458 y contracuenta 721458 (Retiro issste)
               IF ban29128_sie5 = 2 THEN
                  IF (reg_4.cuenta = 721458 AND
                      reg_4.sumarizadora = '031') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711458 AND
                      reg_4.sumarizadora = '062') THEN
                        LET reg_3.importe = 0
                  END IF
                  IF (reg_4.cuenta = 711458 AND
                      reg_4.sumarizadora = '117') THEN
                        LET reg_3.importe = vimporte_29128_sie5
                  END IF
                  IF (reg_4.cuenta = 721458 AND
                      reg_4.sumarizadora = '148') THEN
                        LET reg_3.importe = vimporte_29128_sie5
                  END IF
               END IF
            END IF

            {LET imp_1101 =   
               vimporte_21315_sie1 + 
               vimporte_21315_sie2 + 
               vimporte_21315_sie3 + 
               vimporte_21315_sie4 + 
               vimporte_21315_sie5 +
               vimporte_22332_sie1 + 
               vimporte_22332_sie2 +
               vimporte_22332_sie3 + 
               vimporte_22332_sie4 +
               vimporte_22332_sie5 +
               vimporte_23351_sie1 +
               vimporte_23351_sie2 +
               vimporte_23351_sie3 +
               vimporte_23351_sie4 +
               vimporte_23351_sie5 +
               vimporte_29124_sie1 +
               vimporte_29124_sie2 +
               vimporte_29124_sie3 +
               vimporte_29124_sie4 +
               vimporte_29124_sie5 +
               vimporte_29127_sie1 +
               vimporte_29127_sie2 +
               vimporte_29127_sie3 +
               vimporte_29127_sie4 +
               vimporte_29127_sie5 +
               vimporte_29128_sie1 +
               vimporte_29128_sie2 +
               vimporte_29128_sie3 +
               vimporte_29128_sie4 +
               vimporte_29128_sie5}
#<<<<<

            IF ban_00039 = 1 THEN
               IF (reg_4.cuenta = 110201 AND
                   reg_4.sumarizadora = '063') OR
                  (reg_4.cuenta = 21010001 AND
                   reg_4.sumarizadora = '064') OR
                  (reg_4.cuenta = 110201 AND
                   reg_4.sumarizadora = '086') OR
                  (reg_4.cuenta = 21010001 AND
                   reg_4.sumarizadora = '085')THEN
                     --LET reg_3.importe = importe2101_39
                     LET reg_3.importe = imp_1101_sal
               END IF

               {IF (reg_4.cuenta = 110201 AND
                   reg_4.sumarizadora = '055') OR
                  (reg_4.cuenta = 110201 AND
                   reg_4.sumarizadora = '056') OR
                  (reg_4.cuenta = 110201 AND
                   reg_4.sumarizadora = '057') OR
                  (reg_4.cuenta = 110201 AND
                   reg_4.sumarizadora = '058') OR
                  (reg_4.cuenta = 110201 AND
                   reg_4.sumarizadora = '059') OR
                  (reg_4.cuenta = 123210 AND
                   reg_4.sumarizadora = '060') OR
                  (reg_4.cuenta = 123220 AND
                   reg_4.sumarizadora = '061') OR
                  (reg_4.cuenta = 123230 AND
                   reg_4.sumarizadora = '062') OR
                  (reg_4.cuenta = 123240 AND
                   reg_4.sumarizadora = '063') OR
                  (reg_4.cuenta = 123250 AND
                   reg_4.sumarizadora = '064')THEN
                     LET reg_3.importe = 0
               END IF}
            END IF

---<fin neteo de cuentas de detalle 11 Sep 2008
               SELECT SUM(importe)
               INTO  imp_tot_tras_sie1
               FROM  con_transaccion
               WHERE fecha_emision   = vfecha_reporte
               AND   siefore         = 1
               AND   transaccion_cod <> '99991'
               AND   proceso_cod     = '00039'
               AND   identificador   = 2

               SELECT SUM(importe)
               INTO  imp_tot_tras_sie2
               FROM  con_transaccion
               WHERE fecha_emision   = vfecha_reporte
               AND   siefore         = 2
               AND   transaccion_cod <> '99991'
               AND   proceso_cod     = '00039'
               AND   identificador   = 2

               SELECT SUM(importe)
               INTO  imp_tot_tras_sie3
               FROM  con_transaccion
               WHERE fecha_emision   = vfecha_reporte
               AND   siefore         = 3
               AND   transaccion_cod <> '99991'
               AND   proceso_cod     = '00039'
               AND   identificador   = 2

               SELECT SUM(importe)
               INTO  imp_tot_tras_sie4
               FROM  con_transaccion
               WHERE fecha_emision   = vfecha_reporte
               AND   siefore         = 4
               AND   transaccion_cod <> '99991'
               AND   proceso_cod     = '00039'
               AND   identificador   = 2

               SELECT SUM(importe)
               INTO  imp_tot_tras_sie5
               FROM  con_transaccion
               WHERE fecha_emision   = vfecha_reporte
               AND   siefore         = 5
               AND   transaccion_cod <> '99991'
               AND   proceso_cod     = '00039'
               AND   identificador   = 2

               IF imp_tot_tras_sie1 <> 0 THEN
                  IF imp_tot_tras_sie1 < 0 THEN
                     LET vban_salida1 = 1     #salida
                  ELSE
                     LET vban_salida1 = 11    #entrada
                  END IF
               ELSE
                  LET vban_salida1 = 0
               END IF

               IF imp_tot_tras_sie2 <> 0 THEN
                  IF imp_tot_tras_sie2 < 0 THEN
                     LET vban_salida2 = 2
                  ELSE
                     LET vban_salida2 = 12
                  END IF
               ELSE
                  LET vban_salida2 = 0
               END IF

               IF imp_tot_tras_sie3 <> 0 THEN
                  IF imp_tot_tras_sie3 < 0 THEN
                     LET vban_salida3 = 3
                  ELSE
                     LET vban_salida3 = 13
                  END IF
               ELSE
                  LET vban_salida3 = 0
               END IF

               IF imp_tot_tras_sie4 <> 0 THEN
                  IF imp_tot_tras_sie4 < 0 THEN
                     LET vban_salida4 = 4
                  ELSE
                     LET vban_salida4 = 14
                  END IF
               ELSE
                  LET vban_salida4 = 0
               END IF

               IF imp_tot_tras_sie5 <> 0 THEN
                  IF imp_tot_tras_sie5 < 0 THEN
                     LET vban_salida5 = 5
                  ELSE
                     LET vban_salida5 = 15
                  END IF
               ELSE
                  LET vban_salida5 = 0
               END IF

               IF imp_tot_tras_sie1 <> 0 THEN
                  IF imp_tot_tras_sie1 < 0 THEN
                     LET vban_salida1 = 1
                  ELSE
                     LET vban_salida1 = 11
                  END IF
               ELSE
                  LET vban_salida1 = 0
               END IF

            IF vban_salida1 = 1 THEN
               IF (reg_4.cuenta = 110201 AND
                   reg_4.sumarizadora = '154') OR
                  (reg_4.cuenta = 123210 AND
                   reg_4.sumarizadora = '149') OR
                  (reg_4.cuenta = 710710 AND
                   reg_4.sumarizadora = '159') OR
                  (reg_4.cuenta = 720710 AND
                   reg_4.sumarizadora = '164') THEN
                     LET reg_3.importe = 0
               END IF
            END IF
            IF vban_salida2 = 2 THEN
               IF (reg_4.cuenta = 123220 AND
                   reg_4.sumarizadora = '150') OR
                  (reg_4.cuenta = 110201 AND
                   reg_4.sumarizadora = '155') OR
                  (reg_4.cuenta = 710720 AND
                   reg_4.sumarizadora = '160') OR
                  (reg_4.cuenta = 720720 AND
                   reg_4.sumarizadora = '165') THEN
                     LET reg_3.importe = 0
               END IF
            END IF
            IF vban_salida3 = 3 THEN
               IF (reg_4.cuenta = 123230 AND
                   reg_4.sumarizadora = '151') OR
                  (reg_4.cuenta = 110201 AND
                   reg_4.sumarizadora = '156') OR
                  (reg_4.cuenta = 710730 AND
                   reg_4.sumarizadora = '161') OR
                  (reg_4.cuenta = 720730 AND
                   reg_4.sumarizadora = '166') THEN
                     LET reg_3.importe = 0
               END IF
            END IF
            IF vban_salida4 = 4 THEN
               IF (reg_4.cuenta = 123240 AND
                   reg_4.sumarizadora = '152') OR
                  (reg_4.cuenta = 110201 AND
                   reg_4.sumarizadora = '157') OR
                  (reg_4.cuenta = 710740 AND
                   reg_4.sumarizadora = '162') OR
                  (reg_4.cuenta = 720740 AND
                   reg_4.sumarizadora = '167') THEN
                     LET reg_3.importe = 0
               END IF
            END IF
            IF vban_salida5 = 5 THEN
               IF (reg_4.cuenta = 123250 AND
                   reg_4.sumarizadora = '153') OR
                  (reg_4.cuenta = 110201 AND
                   reg_4.sumarizadora = '158') OR
                  (reg_4.cuenta = 710750 AND
                   reg_4.sumarizadora = '163') OR
                  (reg_4.cuenta = 720750 AND
                   reg_4.sumarizadora = '168') THEN
                     LET reg_3.importe = 0
               END IF
            END IF
----------------------------------------------------<
            IF vban_salida1 = 11 THEN
               IF (reg_4.cuenta = 110201 AND
                   reg_4.sumarizadora = '065') OR
                  (reg_4.cuenta = 123210 AND
                   reg_4.sumarizadora = '070') OR
                  (reg_4.cuenta = 710710 AND
                   reg_4.sumarizadora = '080') OR
                  (reg_4.cuenta = 720710 AND
                   reg_4.sumarizadora = '075') THEN
                     LET reg_3.importe = 0
               END IF
            END IF 
            IF vban_salida2 = 12 THEN
               IF (reg_4.cuenta = 123220 AND
                   reg_4.sumarizadora = '071') OR
                  (reg_4.cuenta = 110201 AND
                   reg_4.sumarizadora = '066') OR
                  (reg_4.cuenta = 710720 AND
                   reg_4.sumarizadora = '081') OR
                  (reg_4.cuenta = 720720 AND
                   reg_4.sumarizadora = '076') THEN
                     LET reg_3.importe = 0
               END IF
            END IF
            IF vban_salida3 = 13 THEN
               IF (reg_4.cuenta = 123230 AND
                   reg_4.sumarizadora = '072') OR
                  (reg_4.cuenta = 110201 AND
                   reg_4.sumarizadora = '067') OR
                  (reg_4.cuenta = 710730 AND
                   reg_4.sumarizadora = '082') OR
                  (reg_4.cuenta = 720730 AND
                   reg_4.sumarizadora = '077') THEN
                     LET reg_3.importe = 0
               END IF
            END IF
            IF vban_salida4 = 14 THEN
               IF (reg_4.cuenta = 123240 AND
                   reg_4.sumarizadora = '073') OR
                  (reg_4.cuenta = 110201 AND
                   reg_4.sumarizadora = '068') OR
                  (reg_4.cuenta = 710740 AND
                   reg_4.sumarizadora = '083') OR
                  (reg_4.cuenta = 720740 AND
                   reg_4.sumarizadora = '078') THEN
                     LET reg_3.importe = 0
               END IF
            END IF 
            IF vban_salida5 = 15 THEN
               IF (reg_4.cuenta = 123250 AND
                   reg_4.sumarizadora = '074') OR
                  (reg_4.cuenta = 110201 AND
                   reg_4.sumarizadora = '069') OR
                  (reg_4.cuenta = 710750 AND
                   reg_4.sumarizadora = '084') OR
                  (reg_4.cuenta = 720750 AND
                   reg_4.sumarizadora = '079') THEN
                     LET reg_3.importe = 0
               END IF
            END IF

         END IF 
------------------------<--------------------------<

          LET reg_4.proceso_cod     = reg_3.proceso_cod
          LET vcuenta               = reg_4.cuenta
--          LET reg_4.transaccion_cod = reg_3.transaccion_cod
          
          LET reg_4.identificador   = reg_3.identificador
--->15 Junio 2006
          IF reg_3.importe = 0 THEN
            CONTINUE FOREACH
          END IF
---<



--->erm 07 Junio 2006
          IF (reg_4.analisis_cod IS NULL OR
             reg_4.analisis_cod = ''    OR
             reg_4.analisis_cod = ' ')   THEN
               LET reg_4.analisis_cod = '0'
          END IF
---<
          LET reg_4.importe = reg_3.importe

         OUTPUT TO REPORT listado_4(reg_4.*) #l3

{--->erm 11 ene 2006
             INSERT INTO safre_tmp:suma_cuentas 
             VALUES (reg_3.proceso_cod,
                     reg_4.cuenta,
                     reg_3.siefore,
                     reg_3.importe,
                     reg_4.id_cuenta,
                     reg_4.descripcion,
                     reg_4.sumarizadora)
---<erm 11 ene 2006}

        END FOREACH

        FINISH REPORT listado_4
        FINISH REPORT listado_3

        UPDATE con_transaccion
        SET    estado        = 40
        WHERE  fecha_emision = vfecha_reporte
        AND    estado        = 20

--->erm 03 Enero 2006
        LET vfecha_gen = CURRENT HOUR TO SECOND 
        INSERT INTO safre_tmp:nombre_archivo 
        VALUES("con"          ,--modulo
               vfecha_reporte ,--fecha_emision
               nombre_archivo ,--nomb_archivo
               vnum_max       ,--consecutivo
               "vig"          ,--estado (vigente)
               ""             ,--fecha_rev
               vfecha_gen     ,--fecha_gen
               g_usuario      )--usuario
---< erm 03 Enero 2006

#---copia para ING
        LET vcopia = "cp ",G_LISTA CLIPPED," ",
                   g_paramgrales.ruta_envio CLIPPED,"/" CLIPPED,"CONSAFRE.csv"
        RUN vcopia

        LET ejecuta = "echo ",vnombre CLIPPED," > ",g_paramgrales.ruta_envio CLIPPED,"/rescate.con "
        RUN ejecuta

        LET mod_perm = 'chmod 777 ',G_TRADUCTOR CLIPPED    ---erm 03 Enero 2006
        RUN mod_perm                                       ---erm 03 Enero 2006

        LET mod_perm2 = 'chmod 777 ',G_LISTA CLIPPED        ---erm 03 Enero 2006
        RUN mod_perm2                                       ---erm 03 Enero 2006

        DISPLAY "ARCHIVO POLIZA GENERADO EN:", G_TRADUCTOR AT 15,1
        --DISPLAY "ARCHIVO SAFRE  GENERADO EN: ", G_LISTA     AT 16,1
        ERROR ""
        PROMPT " PROCESO FINALIZADO...<ENTER> PARA CONTINUAR "
        FOR enter
        EXIT INPUT

      ON KEY(CONTROL-C)
         ERROR " PROCESO CANCELADO "
         SLEEP 1
         EXIT INPUT

      ON KEY(INTERRUPT)
         ERROR " PROCESO CANCELADO "
         SLEEP 1
         EXIT INPUT

    END INPUT 
    CLOSE WINDOW ventana_2 

END FUNCTION
REPORT listado_3(reg_3)
#l3--------------------
    DEFINE reg_3 RECORD #glo #reg_3
        fecha_emision     DATE,
        fecha_valor       DATE,
        identificador     SMALLINT,
        transaccion_cod   INTEGER,
        siefore           SMALLINT,
        importe           DECIMAL(15,2),
        proceso_cod       CHAR(05),
--        folio             INTEGER,
        numero_proceso    SMALLINT,
        vcuenta           CHAR(09)
    END RECORD

    OUTPUT
        PAGE LENGTH 1
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0

    FORMAT

    ON EVERY ROW
--->erm 13 ene 2006
    DECLARE c_cur_6 CURSOR FOR
    SELECT DISTINCT(@a.transaccion_cod)
    FROM   con_traductor a,con_transaccion b
    WHERE  @a.proceso_cod     = reg_3.proceso_cod
    AND    @a.cuenta          = reg_3.vcuenta
    AND    @a.proceso_cod     = @b.proceso_cod
    AND    @a.transaccion_cod = @b.transaccion_cod
    FOREACH c_cur_6  INTO   reg_3.transaccion_cod
       IF reg_3.transaccion_cod IS NOT NULL OR
          reg_3.transaccion_cod <> " " THEN
            EXIT FOREACH
       END IF
    END FOREACH
---<erm 13 ene 2006

        PRINT 
            COLUMN 001,"001",                                 #origen
            COLUMN 004,",",                                  #separador
            COLUMN 005,reg_3.fecha_emision USING"DDMMYYYY", #fecha_emision
            COLUMN 013,",",                                   #separador
            COLUMN 014,reg_3.fecha_valor   USING"DDMMYYYY", #fecha_emision
            COLUMN 022,",",                                   #separador
            COLUMN 023,reg_3.identificador USING"&",          #identificador
            COLUMN 024,",",                                   #separador
            COLUMN 025,reg_3.transaccion_cod USING"&&&&&",    #transaccion
            COLUMN 030,",",                                   #separador
            COLUMN 031,reg_3.importe*100 USING"&&&&&&&&&&&&&&&",  #importe
            COLUMN 046,",",                                   #separador
            COLUMN 047,reg_3.proceso_cod,                     #proceso
            COLUMN 051,",",                                   #separador
            #COLUMN 052,"0",
            COLUMN 052,reg_3.numero_proceso USING"&",
            COLUMN 053,",",                                   #separador
            COLUMN 054,"1"
END REPORT
REPORT listado_4(reg_4)
#l4--------------------
    DEFINE reg_4 RECORD #glo #reg_4
        proceso_cod       CHAR(05)     ,
        transaccion_cod   INTEGER      ,
        tipo              CHAR(01)     ,
        identificador     SMALLINT     ,
        descripcion       CHAR(80)     ,
        cuenta            CHAR(09)     ,
        analisis_cod      CHAR(06)     ,
        importe           DECIMAL(15,2),
        id_cuenta         INTEGER      ,
        sumarizadora      CHAR(3)      ---erm 11 ene 2006
    END RECORD
    DEFINE
        signo             CHAR(01)     ,
        sig_imp           CHAR(18)     ,
        vconsec           INTEGER      ,
        vdescripcion      CHAR(80)     ,
        vtipo_tran        CHAR(2)      ,
        vcuenta           CHAR(10)     ,
        vcar_cre          CHAR(1)      ,
        vauxiliar         INTEGER      ,
        vconsec09         INTEGER

    OUTPUT
      PAGE   LENGTH 1
      LEFT   MARGIN 0
      RIGHT  MARGIN 0
      TOP    MARGIN 0
      BOTTOM MARGIN 0
      ORDER external BY reg_4.proceso_cod, --reg_4.id_cuenta
                        reg_4.sumarizadora

    FORMAT
         BEFORE GROUP OF reg_4.proceso_cod
           LET vconsec = vconsec + 1

         SELECT @descripcion
           INTO vdescripcion
           FROM tab_proceso
          WHERE @proceso_cod = reg_4.proceso_cod

          IF ban_neteo = 1 AND 
            (reg_4.proceso_cod = '00009' OR 
             reg_4.proceso_cod = '00010') THEN
              LET vdescripcion = "TRASPASO AFORE-AFORE COMO RECEPTORA Y CEDENTE"
              IF reg_4.proceso_cod = '00009' THEN
                 LET vconsec09 = vconsec
              ELSE
                 LET vconsec = vconsec09
              END IF
          END IF

      ON EVERY ROW
{ ---> erm 18 Mayo 2006
         --Auxiliar
         LET vauxiliar = 0

         IF reg_4.cuenta = '21010001' THEN
            LET vauxiliar = 1500
         END IF

         IF reg_4.cuenta = '110201' THEN
            LET vauxiliar = 1005
         END IF

         IF reg_4.cuenta = '110202' THEN
            LET vauxiliar = 1008
         END IF
}
         IF reg_4.proceso_cod = 00001 OR
            reg_4.proceso_cod = 00002 OR
            reg_4.proceso_cod = 00003 OR
            reg_4.proceso_cod = 00004 OR
            reg_4.proceso_cod = 00005 OR
            reg_4.proceso_cod = 00006 OR
            reg_4.proceso_cod = 00007 OR
            reg_4.proceso_cod = 00008 OR
            --reg_4.proceso_cod = 00016 OR
            reg_4.proceso_cod = 00033 OR      ---erm 04 Ago 2006
            reg_4.proceso_cod = 00047 OR      ---erm 18 Diciembre 2007
            reg_4.proceso_cod = 00048 OR      ---erm 19 Abril 2007
            reg_4.proceso_cod = 00035 THEN
            LET vtipo_tran = 'RC'
         END IF

         IF reg_4.proceso_cod = 00012 OR
            reg_4.proceso_cod = 00013 OR
            reg_4.proceso_cod = 00014 OR
            reg_4.proceso_cod = 00015 OR
            reg_4.proceso_cod = 00018 OR
            reg_4.proceso_cod = 00019 OR
            reg_4.proceso_cod = 00023 OR
            reg_4.proceso_cod = 00024 OR
            reg_4.proceso_cod = 00031 OR
            reg_4.proceso_cod = 00032 OR
            reg_4.proceso_cod = 00034 OR 
            reg_4.proceso_cod = 00049 THEN        ---erm 12 Julio 2007
--            reg_4.proceso_cod = 00035 THEN
            LET vtipo_tran = 'RT'
            LET reg_4.importe = reg_4.importe * -1
         END IF

         IF reg_4.proceso_cod = 00060 OR
            reg_4.proceso_cod = 00061 OR
            reg_4.proceso_cod = 00062 OR
            reg_4.proceso_cod = 00063 OR
            reg_4.proceso_cod = 00064 OR
            reg_4.proceso_cod = 00065 OR
            reg_4.proceso_cod = 00066 OR
            reg_4.proceso_cod = 00067 OR
            reg_4.proceso_cod = 00068 OR
            reg_4.proceso_cod = 00069 OR
            reg_4.proceso_cod = 00070 OR
            reg_4.proceso_cod = 00071 OR
            reg_4.proceso_cod = 00072 OR
            reg_4.proceso_cod = 00073 OR
            reg_4.proceso_cod = 00074 THEN
            LET vtipo_tran = 'RT'
            LET reg_4.importe = reg_4.importe * -1
         END IF

         CASE reg_4.proceso_cod
           WHEN 00009
              LET vtipo_tran    = 'TR'
           WHEN 00010
              LET vtipo_tran    = 'TR'
              LET reg_4.importe = reg_4.importe * -1
           WHEN 00016                                  ---erm 27 Abril 2007
              LET vtipo_tran    = 'TR'                 ---erm 27 Abril 2007
              LET reg_4.importe = reg_4.importe * -1   ---erm 27 Abril 2007
           WHEN 00021
              LET vtipo_tran    = 'TR'
           WHEN 00026
              LET vtipo_tran    = 'TR'
           WHEN 00029
              LET vtipo_tran    = 'TR'
              LET reg_4.importe = reg_4.importe * -1
           WHEN 00030
              LET vtipo_tran    = 'TR'
           WHEN 00033
              LET reg_4.importe = reg_4.importe * -1
           WHEN 00038
              LET vtipo_tran    = 'TR'
           WHEN 00039
              LET vtipo_tran    = 'TR'
              IF reg_4.importe < 0 THEN
                  LET reg_4.importe = reg_4.importe * -1
              END IF
           WHEN 00045
              LET vtipo_tran    = 'TR'
--->erm 18 Diciembre 2007
           WHEN 00047
              IF reg_4.importe < 0 THEN
                  LET reg_4.importe = reg_4.importe * -1
              END IF
---<
           WHEN 00054
              LET vtipo_tran    = 'DI'
           WHEN 00055
              LET vtipo_tran    = 'DI'
           WHEN 00056
              LET vtipo_tran    = 'DI'

         END CASE

         CASE reg_4.tipo
           WHEN "C"
             LET vcar_cre = "1"
           WHEN "A"
             LET vcar_cre = "0"
         END CASE

--->erm 13 ene 2006
         DECLARE c_cur_7 CURSOR FOR
         SELECT @descripcion
           FROM con_traductor a,con_transaccion b
          WHERE @a.proceso_cod     = reg_4.proceso_cod
            AND @cuenta            = reg_4.cuenta
            AND @a.proceso_cod     = @b.proceso_cod
            AND @a.transaccion_cod = @b.transaccion_cod
        FOREACH c_cur_7 INTO reg_4.descripcion
           IF reg_4.descripcion IS NOT NULL or
              reg_4.descripcion <> " " then
               EXIT FOREACH
           END IF
        END FOREACH
---<erm 13 ene 2006

         PRINT

            COLUMN 001, "1",                             #compania
                        ASCII(9),                        #tabulador
                        vtipo_tran,                      #tipo tran
                        ASCII(9),                        #tabulador
                        vconsec,                         #poliza
                        ASCII(9),                        #tabulador
                        vdescripcion CLIPPED,            #concepto pol
                        ASCII(9),                        #tabulador
                        HOY USING "DD/MM/YYYY",          #fecha
                        ASCII(9),                        #tabulador
                        reg_4.cuenta,                    #cuenta MOD19122005
                        ASCII(9),                        #tabulador
                        "1",                             #centro costo
                        ASCII(9),                        #tabulador
                        "1",                             #uen
                        ASCII(9),                        #tabulador
--                        vauxiliar,                       #auxiliar
                        reg_4.analisis_cod,              #analisis_cod=auxiliar --18 Mayo 2006
                        ASCII(9),                        #tabulador
                        "*",                             #proyecto
                        ASCII(9),                        #tabulador
                        "CG",                            #libro
                        ASCII(9),                        #tabulador
                        "1",                             #moneda
                        ASCII(9),                        #tabulador
                        reg_4.importe,                   #importe capturado
                        ASCII(9),                        #tabulador
                        reg_4.importe,                   #importe convertido
                        ASCII(9),                        #tabulador
                        vcar_cre,                        #cargo credito
                        ASCII(9),                        #tabulador
                        "0",                             #unidades
                        ASCII(9),                        #tabulador
                        "1",                             #tipo cambio
                        ASCII(9),                        #tabulador
                        reg_4.descripcion CLIPPED,       #concepto mov
                        ASCII(9),                        #tabulador
                        " ",                             #cheque
                        ASCII(9),                        #tabulador
                        " "                              #referencia

END REPORT

FUNCTION reversa()
#c-----------------
    DEFINE vaccion_safre     DECIMAL(15,2)
    DEFINE vprecio_safre     DECIMAL(15,2)
    DEFINE vaccion_tesoreria DECIMAL(15,2)
    DEFINE vprecio_tesoreria DECIMAL(15,2)
    DEFINE vprecio           DECIMAL(10,6)
    DEFINE vdif_concilia     DECIMAL(5,2)
    DEFINE vdif_fraccion     DECIMAL(5,2)
    DEFINE vreverso1         CHAR(01)
    DEFINE vreverso2         CHAR(01)
    DEFINE aux_pausa         CHAR(01)
    DEFINE vproceso          CHAR(05)
    DEFINE vdescripcion      CHAR(40)
    DEFINE vdesc_estado      CHAR(40)
    DEFINE vmovimiento       SMALLINT
    DEFINE vfecha            DATE
    DEFINE vestado           SMALLINT
    DEFINE videntificador    SMALLINT
    DEFINE vtransaccion      SMALLINT
    DEFINE vacciones         SMALLINT
    DEFINE vpesos            SMALLINT
    DEFINE ventero           INTEGER
    DEFINE vfolio_rev        INTEGER
    DEFINE vcap              CHAR(1)

    DEFINE vnombre_archivo   CHAR(30)
    DEFINE g_reversa         CHAR(120)
    DEFINE g_reversa2        CHAR(120)
    DEFINE vnombre2          CHAR(12)
    DEFINE consec            CHAR(5)
    DEFINE vfecha_rev        DATETIME HOUR TO SECOND 

    INITIALIZE vproceso     TO NULL
    INITIALIZE vdescripcion TO NULL
    INITIALIZE vfolio       TO NULL
    INITIALIZE vfecha       TO NULL
    INITIALIZE vestado      TO NULL
    LET vacciones      = 2
    LET vpesos         = 1
    LET vfecha_rev     = CURRENT HOUR TO SECOND 

    OPEN WINDOW ventana_3 AT 5,2 WITH FORM "CONM0014" ATTRIBUTE(BORDER)
    DISPLAY "          [ Esc ] Iniciar                            [ Ctrl-C ] Salir          " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "CONM001         REVERSA PROCESOS REGISTRADOS Y CONCILIADOS                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    INPUT BY NAME vproceso,
                  vdescripcion,
                  vfecha,
                  vestado,
                  vdesc_estado,
                  vreverso1,
                  vreverso2   WITHOUT DEFAULTS

        AFTER  FIELD vproceso

           IF vproceso IS NULL THEN
               CALL despliega_tipo()
               RETURNING vproceso,vdescripcion
           END IF

           IF vproceso IS NULL THEN
               ERROR "Proceso no puede ser NULLO"
               SLEEP 3
               NEXT FIELD vproceso
           END IF

             SELECT "X"
             FROM   tab_proceso
             WHERE  proceso_cod = vproceso

               IF STATUS <> NOTFOUND THEN
                   SELECT descripcion
                   INTO   vdescripcion
                   FROM   tab_proceso
                   WHERE  proceso_cod = vproceso
               ELSE
                   ERROR "No existe este tipo de proceso" 
                   SLEEP 3
                   NEXT FIELD vproceso
               END IF

           DISPLAY "DESCRIPCION DEL PROCESO" AT  7,16 ATTRIBUTE(REVERSE)
           DISPLAY BY NAME vproceso,
                           vdescripcion

        AFTER  FIELD vfecha

           IF vfecha IS NULL THEN
               ERROR "La fecha no puede ser nula"
               SLEEP 2
               ERROR ""
               NEXT FIELD vfecha
           ELSE
               SELECT "X"
               FROM   con_transaccion
               WHERE  proceso_cod = vproceso
               AND    fecha_emision = vfecha
               GROUP BY 1
  
               IF STATUS = NOTFOUND THEN
                   ERROR "No existe proceso con esta fecha"
                   SLEEP 3
                   NEXT FIELD vfecha
               END IF

               SELECT estado
               INTO   vestado
               FROM   con_transaccion
               WHERE  proceso_cod   = vproceso
               AND    fecha_emision = vfecha
               GROUP BY 1

               IF vestado = 40 THEN
                   ERROR "Este proceso ya esta Contabilizado..."
                   SLEEP 2
--=>erm reversa archivo
                   --EXIT PROGRAM
               --END IF
                  OPEN WINDOW ventana_x AT 9,4 WITH 3 rows, 74 COLUMNS ATTRIBUTE (BORDER)
                  DISPLAY "                            REVERSA ARCHIVO                               " AT 1,1 ATTRIBUTE (REVERSE)
                  WHILE TRUE 
                  PROMPT "Desea reversar la Generacion de Archivo de Fecha Emision " ,vfecha USING "DD/MM/YYYY","? S/N " FOR vcap
                     IF vcap matches"[SsNn]" THEN
                        EXIT WHILE
                     END IF
                  END WHILE
                  CLOSE WINDOW ventana_x
                     IF vcap MATCHES "[Ss]" THEN
                        ERROR "Reversando Generación de Archivo, pasa a Estado 20..."
                        SLEEP 3

      --->erm 03 Enero 2006
                        DECLARE c_nombarch CURSOR FOR 
                        SELECT nombre_archivo,consecutivo
                        FROM   safre_tmp:nombre_archivo
                        WHERE  fecha_emision = vfecha
                        FOREACH c_nombarch INTO vnombre_archivo,consec

                           UPDATE safre_tmp:nombre_archivo 
                           SET    estado    = "rev",       --- (reversado)
                                  fecha_rev = vfecha_rev,
                                  usuario   = g_usuario
                           WHERE  fecha_emision  = vfecha
                           AND    nombre_archivo = vnombre_archivo
                           AND    estado         = "vig"
      ---<erm 03 Enero 2006
                           UPDATE con_transaccion
                           SET    estado = 20
                           WHERE  fecha_emision = vfecha
                           AND    estado = 40
                           --ERROR "EL REVERSO DE GENERACION DE ARCHIVO DEL ",vfecha USING "DD/MM/YYYY"," SE HA REALIZADO "
                           ERROR "OPERANDO REVERSO"
                           --SLEEP  3 
                           --ERROR ""
       --->erm 03 Enero 2006
                           SELECT * 
                           INTO   g_paramgrales.*
                           FROM   seg_modulo
                           WHERE  modulo_cod = "con"

                           LET g_reversa = "mv ",g_paramgrales.ruta_envio CLIPPED,"/" CLIPPED,
                                           vnombre_archivo CLIPPED,".txt"," ",
                                           g_paramgrales.ruta_envio CLIPPED,"/" CLIPPED,
                                           vnombre_archivo CLIPPED,".rev"
                           RUN g_reversa

                           LET vnombre2  = vnombre_archivo [10,15]
                           LET g_reversa2 = "mv ",g_paramgrales.ruta_envio CLIPPED,"/" CLIPPED,
                                            "In",vnombre2 CLIPPED,"_",consec CLIPPED, ".txt" CLIPPED," ",
                                            g_paramgrales.ruta_envio CLIPPED,"/" CLIPPED,
                                            "In",vnombre2 CLIPPED,"_",consec CLIPPED,".rev" CLIPPED
                           RUN g_reversa2 
                        END FOREACH
       ---<erm 03 Enero 2006
                        ERROR "EL REVERSO DE GENERACION DE ARCHIVO DEL ",vfecha USING "DD/MM/YYYY"," SE HA REALIZADO "
                        SLEEP 3
                        ERROR ""
                        EXIT INPUT
                        CLOSE WINDOW ventana_3
                        RETURN
                     ELSE
                        ERROR "Reverso NO requerido... "
                        SLEEP 3
                        EXIT PROGRAM
                     END IF
               END IF
--<=erm reversa archivo
               SELECT descripcion
               INTO   vdesc_estado
               FROM   con_status
               WHERE  status = vestado

               DISPLAY BY NAME vestado
               DISPLAY BY NAME vdesc_estado

           END IF

        AFTER  FIELD vreverso1

           IF vreverso1 = "N"   THEN
               ERROR "No Requiere Realizar Reverso Total de la Operacion"
               SLEEP 2
               ERROR ""
               NEXT FIELD vreverso2
           END IF
           IF vreverso1 = "S" THEN
               WHILE TRUE
                   PROMPT "Desea Realizar Reverso Total S/N ? "
                   FOR CHAR aux_pausa
                   IF aux_pausa MATCHES "[SsNn]" THEN
                        EXIT WHILE
                   END IF
               END WHILE
               IF aux_pausa MATCHES "[Ss]" THEN
                   ERROR "Procesando Informacion ... Espere un momento"
		       LET  vfolio_rev = 0
		       DECLARE cur_rev CURSOR FOR
		           SELECT folio
		           FROM   con_transaccion
                           WHERE  proceso_cod   = vproceso
                           AND    fecha_emision = vfecha
		           GROUP BY 1
		       FOREACH cur_rev INTO vfolio_rev

                           DELETE
                           FROM   con_transaccion
                           WHERE  proceso_cod   = vproceso
                           AND    fecha_emision = vfecha
                           AND    estado        <> 40
  
                           DELETE
                           FROM   con_transaccion
                           WHERE  proceso_cod   = vproceso
                           AND    folio         = vfolio_rev
		           AND    estado        <> 40
                       END FOREACH

		       IF vproceso = "00022" THEN
			   DELETE 
                           FROM   con_transaccion
                           WHERE  proceso_cod   = "00027"
                           AND    fecha_emision = vfecha
                           AND    estado        <> 40
		       END IF
  

                   ERROR "EL REVERSO DEL PROCESO SE HA REALIZADO"
                   SLEEP 3
                   ERROR " "
                   EXIT INPUT
               END IF
           END IF
  
        AFTER  FIELD vreverso2

           IF vreverso2 = "N"   THEN
               ERROR "No Requiere Realizar ningun Reverso"
               SLEEP 2
               ERROR ""
               EXIT PROGRAM
           END IF
           IF vreverso2 = "S" THEN
               WHILE TRUE
                   PROMPT "Desea Realizar Reverso Parcial S/N ? "
                   FOR CHAR aux_pausa
                   IF aux_pausa MATCHES "[SsNn]" THEN
                        EXIT WHILE
                   END IF
               END WHILE
               IF aux_pausa MATCHES "[Ss]" THEN
                   CASE vestado
                       WHEN 10
		           LET  vfolio_rev = 0

		           SELECT folio
		           INTO   vfolio_rev
		           FROM   con_transaccion
                           WHERE  proceso_cod   = vproceso
                           AND    fecha_emision = vfecha
		           GROUP BY 1

                           DELETE 
                           FROM   con_transaccion
                           WHERE  proceso_cod   = vproceso
                           AND    fecha_emision = vfecha
                           AND    estado        = 10

                           DELETE
                           FROM   con_transaccion
                           WHERE  proceso_cod   = vproceso
                           AND    folio         = vfolio_rev
                           AND    estado        in(10,20)

		           IF vproceso = "00022" THEN
			       DELETE 
                               FROM   con_transaccion
                               WHERE  proceso_cod   = "00027"
                               AND    fecha_emision = vfecha
                               AND    estado        in(10,20)
		           END IF
  
                       WHEN 20
		           IF vproceso = "00022" THEN
			       DELETE 
                               FROM   con_transaccion
                               WHERE  proceso_cod   in("00022","00027")
                               AND    fecha_emision = vfecha
                               AND    estado        = 20 
		           END IF
                           DELETE 
                           FROM   con_transaccion
                           WHERE  proceso_cod   = vproceso
                           AND    fecha_emision = vfecha
                           AND    estado        = 20
                           #AND    transaccion_cod IN (51000,53000)
                           AND    transaccion_cod = 99991
 
                           SELECT MIN(transaccion_cod)
                           INTO   vtransaccion
                           FROM   con_transaccion
                           WHERE  proceso_cod   = vproceso
                           AND    fecha_emision = vfecha
                           AND    estado        = 20

                           SELECT movimiento
                           INTO   vmovimiento
                           FROM   tab_proceso
                           WHERE  proceso_cod = vproceso
  
                           IF vmovimiento = 1 THEN
                               LET videntificador = 2
                               DELETE 
                               FROM   con_transaccion
                               WHERE  proceso_cod     = vproceso
                               AND    fecha_emision   = vfecha
                               AND    estado          = 20
                               AND    transaccion_cod = vtransaccion
                               AND    importe         < 1
                               AND    identificador   = videntificador
                           ELSE
                               LET videntificador = 1
                               DELETE 
                               FROM   con_transaccion
                               WHERE  proceso_cod     = vproceso
                               AND    fecha_emision   = vfecha
                               AND    estado          = 20
                               AND    transaccion_cod = vtransaccion
                               AND    importe         < -1
                               AND    identificador   = videntificador
                           END IF

                           UPDATE con_transaccion
                           SET    estado        = 10 
                           WHERE  proceso_cod   = vproceso
                           AND    fecha_emision = vfecha
                           AND    estado        = 20
  
                   END CASE
               ELSE
                   ERROR "PROCESO CANCELADO"
                   SLEEP 2
                   ERROR " "
                   EXIT INPUT
               END IF
           END IF
                    
     ERROR" REVERSO REALIZADO SATISFACTORIAMENTE"
     SLEEP 3
     ERROR ""
     EXIT INPUT


     ON KEY(CONTROL-C)
         ERROR " PROCESO CANCELADO "
         SLEEP 1
         EXIT INPUT

      ON KEY(INTERRUPT)
         ERROR " PROCESO CANCELADO "
         SLEEP 1
         EXIT INPUT

    END INPUT 
    CLOSE WINDOW ventana_3

END FUNCTION
FUNCTION  despliega_tipo()
    DEFINE pos        INTEGER
    DEFINE cla_where  CHAR(200)
    DEFINE sel_where  CHAR(200)
    DEFINE vproceso   CHAR(005)
    DEFINE vdescripcion   CHAR(080)
    DEFINE l_record  ARRAY[1000] OF RECORD
        codigo         CHAR(05),
        descripcion    CHAR(80)
    END RECORD

   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,18 WITH FORM "CONC0012" ATTRIBUTE( BORDER)
      DISPLAY "    (Enter) Seleccionar                (Ctrl-C) Salir      " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "        Escoja con < ENTER > seleccionar el tipo           " AT 2,1
      DISPLAY "                                                           " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
   
      LET int_flag = FALSE

      CONSTRUCT cla_where   ON proceso_cod 
                          FROM proceso_cod

         ON KEY (CONTROL-M)
            LET int_flag = FALSE
            EXIT CONSTRUCT

         ON KEY (CONTROL-C)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         INITIALIZE l_record[1].codigo  TO NULL
         INITIALIZE l_record[1].descripcion  TO NULL
         RETURN l_record[pos].codigo, l_record[pos].descripcion
      END IF

      LET sel_where = "SELECT * FROM tab_proceso WHERE ",cla_where CLIPPED,
                      "ORDER BY 1,2 "
  
      PREPARE query1 FROM sel_where

      DECLARE cursor_2 CURSOR FOR query1

      LET pos = 1
      FOREACH cursor_2 INTO l_record[pos].*
         LET pos = pos +1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
          CALL SET_COUNT(pos-1) 
          DISPLAY ARRAY l_record TO scr_1.* 

             ON KEY (CONTROL-M)
                LET pos = ARR_CURR()
                LET vproceso     = l_record[pos].codigo
                LET vdescripcion = l_record[pos].descripcion
                EXIT DISPLAY

             ON KEY (CONTROL-C)
                ERROR "Debe Seleccionar un registro"
                LET pos = ARR_CURR()

             ON KEY (INTERRUPT)
                ERROR "Debe Seleccionar un registro"
                LET pos = ARR_CURR()

          END DISPLAY
          CLOSE WINDOW ventana_2
       ELSE
          ERROR "EL PROCESO NO EXISTE"
          SLEEP 2
          ERROR ""
          CLOSE WINDOW ventana_2
          RETURN
       END IF

    END IF

    RETURN l_record[pos].codigo, l_record[pos].descripcion
END FUNCTION
