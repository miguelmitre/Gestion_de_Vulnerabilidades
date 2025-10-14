##############################################################################
#Owner             => E.F.P.
#Programa TRAM071  => CONFIRMACION MANUAL INVITACION ISSSTE 
#Fecha creacion    => 04 DE MAYO DE 1999
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 19 DE ENERO DEL 2005
#Ultima Mod        => ENERO DEL 2006.
#Sistema           => TRA-ICE-ISSSTE
##############################################################################
DATABASE safre_af 
GLOBALS
   DEFINE b_window  SMALLINT
   DEFINE ejecuta_p CHAR(100)
   DEFINE b_uaep   SMALLINT
   DEFINE folio_uaep INTEGER
   DEFINE txt      CHAR(1500)
   DEFINE ffb      SMALLINT
   DEFINE x_busca  CHAR(100)
   DEFINE usuario  CHAR(010)
   DEFINE v_mod SMALLINT
   DEFINE rr SMALLINT
   DEFINE vtipo SMALLINT
   DEFINE vestado  SMALLINT
   DEFINE ruta char(100)
   DEFINE reg_tra_det_atm_issste RECORD LIKE tra_det_atm_issste.*
   DEFINE v_corr integer
   DEFINE sw smallint
   DEFINE reg_param_tra RECORD LIKE seg_modulo.*
   DEFINE band  CHAR(01)

   DEFINE #glo #char
         g_enter                 CHAR(1) ,
         enter                 CHAR(1) ,
         HORA                  CHAR(8)
   DEFINE v_folio_interno INTEGER
   DEFINE v_correlativo INTEGER
   DEFINE HOY             DATE

   DEFINE b SMALLINT

   DEFINE  g_glob            RECORD
           codigo_afore      LIKE safre_af:tab_afore_local.codigo_afore,
           razon_social      LIKE safre_af:tab_afore_local.razon_social
                             END RECORD

   DEFINE  g_nom_prog        CHAR(07)

   DEFINE   arr_afi            ARRAY[10]  OF    RECORD
            n_seguro_a         LIKE     afi_mae_afiliado.n_seguro,
            n_rfc_a            LIKE     afi_mae_afiliado.n_rfc   ,
            paterno_a          LIKE     afi_mae_afiliado.paterno ,
            materno_a          LIKE     afi_mae_afiliado.materno ,
            nombres_a          LIKE     afi_mae_afiliado.nombres 
                                           END   RECORD
END GLOBALS  

   DEFINE g_ind_afi            SMALLINT
MAIN

    DEFER INTERRUPT      
    OPTIONS
        ACCEPT KEY CONTROL-I ,
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        MESSAGE LINE LAST

     CALL init()

   OPEN WINDOW trac0752  AT 2,2 WITH FORM "TRAC0752" ATTRIBUTE(BORDER)

    MENU "CONTROL INVITACIONES ISSSTE"
    COMMAND "Individual" "Confirmacion individual de registros"
        CALL despliega_folios()
  #========SE COMENTO ESTA PARTE YA Q ANTERIORMENTE CARGABAN UN ARCHIVO EN 
  #        LAS AFORES PERO YA NO.
  # COMMAND "Archivo Plano" "Confirmacion con archivo plano de correlativos"
  #    CALL uno() #c
  #=======================================================================
    COMMAND "Consulta" "Consulta Estado de registros "
       CALL consulta_estados() #c
    COMMAND "Salir" "Salir del Programa"
          EXIT MENU
  END MENU

END MAIN

FUNCTION despliega_folios()
#-------------------------


DEFINE f_b SMALLINT
DEFINE arr_tra_ctr_folio ARRAY[100] OF RECORD LIKE tra_ctr_folio.*

DEFINE reg_tra_ctr_folio RECORD LIKE tra_ctr_folio.*

DEFINE arr_c                ,
       i             SMALLINT


LET f_b = 1
    OPEN WINDOW tram0711 AT 2,2 WITH FORM "TRAM0711" ATTRIBUTE(BORDER)
    DISPLAY" TRAM0711   CONFIRMACION DE INVITACION SAR92 ICEFA-AFORE ISSSTE                " AT 3,1 ATTRIBUTE(REVERSE)   

    DISPLAY "                            < CTRL-C> Salir                                        " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)


    MESSAGE "<ENTER> Elegir folio ..." ATTRIBUTE(REVERSE)

     DECLARE cur_dsply_folio CURSOR FOR
      
     SELECT A.*      
     FROM   tra_ctr_folio A
     ORDER BY A.folio_interno

     LET i = 1

     FOREACH cur_dsply_folio
     INTO reg_tra_ctr_folio.*

          LET  arr_tra_ctr_folio[i].* =
               reg_tra_ctr_folio.*

          LET i = i + 1

     END FOREACH

     CALL SET_COUNT(i-1)

     DISPLAY ARRAY arr_tra_ctr_folio
                TO sr_ctr_folio.*

     ON KEY(RETURN) 
        LET arr_c = ARR_CURR()

        INPUT BY NAME vtipo
        AFTER FIELD vtipo 
          IF vtipo IS NULL THEN 
            ERROR"CAMPO NO PUEDE SER NULO"
            NEXT FIELD vtipo 
          END IF 
          IF (vtipo > 2 OR
             vtipo < 1) THEN
            ERROR"1 ACEPTADAS , 2 RECHAZADAS"
            NEXT FIELD vtipo
          END IF
        ON KEY(ESC)
          IF vtipo IS NULL THEN 
            ERROR"CAMPO NO PUEDE SER NULO"
            NEXT FIELD vtipo 
          END IF 
          IF (vtipo > 2 OR
             vtipo < 1) THEN
            ERROR"1 ACEPTADAS , 2 RECHAZADAS"
            NEXT FIELD vtipo
          END IF

          IF vtipo = 1 THEN
             LET vestado = 110
          ELSE 
             LET vestado = 112
          END IF

    OPEN WINDOW tram0712 AT 2,2 WITH FORM "TRAM0712" ATTRIBUTE(BORDER)
    DISPLAY" TRAM0712       CONFIRMACION INVITACION SAR92 ICEFA-AFORE ISSSTE                    " AT 3,1 ATTRIBUTE(REVERSE)   

    DISPLAY "<F1>Confirmar <F2>Deshacer <CTRL-C>SALIR                                         " AT 2,60 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)
       LET b_window = 0
	MENU "MENU DE ENVIO DE CARTAS INVITACION"
		COMMAND "Confirma" "Confirma envio de cartas invitacion"
         CALL despliega_confronta(arr_tra_ctr_folio[arr_c].folio_interno,
                                  arr_tra_ctr_folio[arr_c].cve_desc,
                                  arr_tra_ctr_folio[arr_c].criterio_cod,
                                  arr_tra_ctr_folio[arr_c].criterio_desc )
           
      COMMAND "Consulta" "Consulta de cartas enviadas"
         CALL despliega_consulta(arr_tra_ctr_folio[arr_c].folio_interno,
                                  arr_tra_ctr_folio[arr_c].cve_desc,
                                  arr_tra_ctr_folio[arr_c].criterio_cod,
                                  arr_tra_ctr_folio[arr_c].criterio_desc )

      COMMAND "Salir" "Salir pantalla anterior"
           EXIT MENU
	END MENU
          CLOSE WINDOW tram0712  
          EXIT INPUT 
          END INPUT
     ON KEY(INTERRUPT)
         LET f_b = 0
        EXIT DISPLAY
     END DISPLAY

   IF f_b = 0 THEN

     SELECT "OK" 
     FROM rep_aut
     GROUP BY 1

     IF STATUS <> NOTFOUND THEN

        START REPORT listado_1 TO ruta

        DECLARE cur_rep CURSOR FOR

         SELECT A.*
         FROM  tra_det_atm_issste A
         WHERE  A.correlativo IN 
               (SELECT unique H.correlativo
                FROM   rep_aut H)
         AND    A.estado IN (160)
         ORDER BY A.estado

         FOREACH cur_rep INTO  reg_tra_det_atm_issste.*
            OUTPUT TO REPORT listado_1(reg_tra_det_atm_issste.*)
         END FOREACH 
         FINISH REPORT  listado_1
     END IF
     CLOSE WINDOW TRAM0711 
   --  EXIT PROGRAM
   END IF
END FUNCTION

FUNCTION despliega_confronta(f_folio,f_cve_desc,f_criterio_cod,
                             f_criterio_desc)
#dc------------------------------------------------------------

DEFINE f_folio          INTEGER
DEFINE f_cve_desc       CHAR(10)
DEFINE x                SMALLINT
DEFINE f_criterio_cod   SMALLINT 
DEFINE lastkey         SMALLINT 
DEFINE f_criterio_desc  CHAR(030)
DEFINE f_correlativo    INTEGER
DEFINE f_icefa_cod      CHAR(05)  

DEFINE farr_tra_det_atm_issste ARRAY[10000] OF RECORD 
       criterio_cod   LIKE tra_tab_valcri.criterio_cod,
       criterio_desc   char(030),
       folio_interno  LIKE tra_det_atm_issste.folio_interno,
       icefa_cod      LIKE tab_icefa.icefa_cod,
       icefa_desc     LIKE tab_icefa.icefa_desc,
       correlativo    LIKE tra_det_atm_issste.correlativo,
       n_seguro       LIKE tra_det_atm_issste.n_seguro,
       n_seguro_ent   LIKE tra_det_atm_issste.n_seguro_ent,
       rfc_ent        LIKE tra_det_atm_issste.rfc_ent,
       usuario        CHAR(010)                            ,
       nro_ctrl_icefa LIKE tra_det_atm_issste.nro_ctrl_icefa,
       x              SMALLINT  ,
       paterno        CHAR(040) ,
       materno        CHAR(040) ,
       nombres        CHAR(040) ,
       sar_92_issste   LIKE tra_det_atm_issste.sar_92_issste ,  
       viv_92_issste   LIKE tra_det_atm_issste.viv_92_issste ,
       estado         LIKE tra_det_atm_issste.estado       ,
       estado_desc    CHAR(040)                            ,
       num_lin        INTEGER                              ,
       n_seguro_a     like afi_mae_afiliado.n_seguro       ,
       n_rfc_a        like afi_mae_afiliado.n_rfc          ,
       paterno_a      like afi_mae_afiliado.paterno        ,
       materno_a      like afi_mae_afiliado.materno        ,
       nombres_a      like afi_mae_afiliado.nombres
END RECORD

DEFINE freg_tra_det_atm_issste         RECORD 
       criterio_cod   LIKE tra_tab_valcri.criterio_cod,
       criterio_desc   char(030),
       folio_interno  LIKE tra_det_atm_issste.folio_interno,
       icefa_cod      LIKE tab_icefa.icefa_cod,
       icefa_desc     LIKE tab_icefa.icefa_desc,
       correlativo    LIKE tra_det_atm_issste.correlativo,
       n_seguro       LIKE tra_det_atm_issste.n_seguro,
       n_seguro_ent   LIKE tra_det_atm_issste.n_seguro_ent,
       rfc_ent        LIKE tra_det_atm_issste.rfc_ent,
       usuario        CHAR(010) ,
       nro_ctrl_icefa LIKE tra_det_atm_issste.nro_ctrl_icefa,
       x              SMALLINT  ,
       paterno        CHAR(040) ,
       materno        CHAR(040) ,
       nombres        CHAR(040) ,
       sar_92_issste   LIKE tra_det_atm_issste.sar_92_issste ,
       viv_92_issste   LIKE tra_det_atm_issste.viv_92_issste ,
       estado         SMALLINT  ,
       estado_desc    CHAR(040) ,
       num_lin        INTEGER                         ,
       n_seguro_a     like afi_mae_afiliado.n_seguro       ,
       n_rfc_a        like afi_mae_afiliado.n_rfc          ,
       paterno_a      like afi_mae_afiliado.paterno        ,
       materno_a      like afi_mae_afiliado.materno        ,
       nombres_a      like afi_mae_afiliado.nombres
END RECORD

DEFINE festado,
       arr_c                ,
       i             SMALLINT,
       tot_reg_pant  SMALLINT 
define v_pru   char(03)


LET ffb = 0

WHILE TRUE
  CLEAR FORM
  CONSTRUCT BY NAME x_busca ON A.rfc_ent

        ON KEY (ESC )
            LET INT_FLAG = FALSE
            EXIT CONSTRUCT

        ON KEY (INTERRUPT )
            LET INT_FLAG = FALSE
	    LET ffb = 1
            EXIT CONSTRUCT
    END CONSTRUCT

   IF ffb = 1 THEN
       LET ffb = 0
       EXIT WHILE
   END IF

     SELECT A.icefa_cod CLIPPED
        INTO f_icefa_cod
        FROM tab_icefa A
     WHERE A.icefa_desc[1,10] = f_cve_desc
     GROUP BY 1


     LET txt= " SELECT MIN(A.correlativo)   ",
              " FROM   tra_det_atm_issste A ",
              " WHERE  A.folio_interno  = ",f_folio CLIPPED,
              " AND    A.estado         = ",vestado CLIPPED,
              " AND    A.cve_ced_cuenta = ",f_icefa_cod CLIPPED , 
              " AND    A.tipo_criterio =  ",f_criterio_cod CLIPPED

     PREPARE qry22 FROM txt

     DECLARE cur CURSOR FOR qry22
     FOREACH cur INTO f_correlativo
  
        EXIT FOREACH

     END FOREACH 


     IF f_correlativo IS NULL THEN 
        ERROR "NO HAY REGISTROS PARA MOSTRAR "
        SLEEP 3
        RETURN
     END IF
     
 
LET txt =       
    'SELECT A.cve_ced_cuenta     ,    ',
    ' A.tipo_criterio            ,    ',
    ' A.folio_interno,                ',
    ' A.correlativo,                  ',
    ' A.n_seguro,                     ',
    ' A.n_seguro_ent ,                ',
    ' A.rfc_ent ,                     ',
    ' A.usuario           ,     ',
    ' A.nro_ctrl_icefa ,              ',
    ' A.nombre_ent[001,040],    ',
    ' A.nombre_ent[041,080],    ',
    ' A.nombre_ent[081,120],    ',
    ' A.sar_92_issste      ,    ',
    ' A.viv_92_issste      ,    ',
    ' A.estado            ,     ',
    ' Z.estado_descripcion      ',
    'FROM   tra_det_atm_issste A, ',
    '       tra_aut_estado     Z  ',
#   '       safre_af:afi_mae_afiliado    C  ',
    ' WHERE  ',x_busca CLIPPED,
    ' AND    A.folio_interno =  ',f_folio       ,
    ' AND    A.correlativo   >= ',f_correlativo ,
    ' AND    A.estado        =  ',vestado       ,
#   ' AND    A.n_seguro      = C.n_seguro      ',
    ' AND    A.estado        = Z.estado_cod    ',
    ' AND    A.cve_ced_cuenta = ',f_icefa_cod CLIPPED , 
    ' AND    A.tipo_criterio  = ',f_criterio_cod CLIPPED,
    ' ORDER BY A.correlativo '
     LET txt = txt CLIPPED
 
     PREPARE qry1 FROM txt

     DECLARE cur_dsply_detalle CURSOR FOR qry1

     LET i = 1
     LET tot_reg_pant   = 0 
     LET freg_tra_det_atm_issste.num_lin       = 1

     FOREACH cur_dsply_detalle
#    INTO  freg_tra_det_atm_issste.*
 
     INTO freg_tra_det_atm_issste.icefa_cod    ,
          freg_tra_det_atm_issste.criterio_cod    ,
          freg_tra_det_atm_issste.folio_interno    ,
          freg_tra_det_atm_issste.correlativo      ,
          freg_tra_det_atm_issste.n_seguro         ,
          freg_tra_det_atm_issste.n_seguro_ent     ,
          freg_tra_det_atm_issste.rfc_ent          ,
          freg_tra_det_atm_issste.usuario          ,
          freg_tra_det_atm_issste.nro_ctrl_icefa   ,
          freg_tra_det_atm_issste.paterno          ,
          freg_tra_det_atm_issste.materno          ,
          freg_tra_det_atm_issste.nombres          ,
          freg_tra_det_atm_issste.sar_92_issste     ,
          freg_tra_det_atm_issste.viv_92_issste     ,
          freg_tra_det_atm_issste.estado           ,
          freg_tra_det_atm_issste.estado_desc
 
      IF    vtipo          =  1      THEN 
          SELECT   C.n_seguro,C.n_rfc,C.paterno,C.materno,C.nombres
            INTO   freg_tra_det_atm_issste.n_seguro_a       ,
                   freg_tra_det_atm_issste.n_rfc_a          ,
                   freg_tra_det_atm_issste.paterno_a        ,
                   freg_tra_det_atm_issste.materno_a        ,
                   freg_tra_det_atm_issste.nombres_a
            FROM   safre_af:afi_mae_afiliado  C
           WHERE   C.n_seguro      =  freg_tra_det_atm_issste.n_seguro;
      END IF
          SELECT icefa_desc 
	  INTO   freg_tra_det_atm_issste.icefa_desc  
	  FROM   tab_icefa 
	  WHERE  icefa_cod = freg_tra_det_atm_issste.icefa_cod

          SELECT vector
	  INTO   freg_tra_det_atm_issste.criterio_desc
	  FROM   tra_tab_valcri 
	  WHERE  criterio_cod = freg_tra_det_atm_issste.criterio_cod

          LET  farr_tra_det_atm_issste[i].* =
               freg_tra_det_atm_issste.*

          LET freg_tra_det_atm_issste.num_lin = 
	      freg_tra_det_atm_issste.num_lin + 1 

          LET i = i + 1
          LET tot_reg_pant = tot_reg_pant + 1
         
                  
          IF i >= 9999 THEN  
             EXIT FOREACH
          END IF
     
     END FOREACH
     

     CALL SET_COUNT(i-1)
    
     DISPLAY "de: ["," ",tot_reg_pant USING "######","]" AT 15, 20
 IF vtipo = 2 THEN
     DISPLAY "<F1>Confirma <F2>Deshacer <F3>Confirma UAEP" AT 21,2 ATTRIBUTE(REVERSE)
     ELSE
     DISPLAY "<F1>Confirma <F2>Deshacer " AT 21,2 ATTRIBUTE(REVERSE)

     END IF

   DISPLAY  ARRAY  farr_tra_det_atm_issste     TO  sr_ctr_folio1.*
     ON  KEY(RETURN)
           LET      arr_c          =  ARR_CURR()
           IF       vtipo          =  2      THEN
                    CALL     F_100_despliega_afiliados (
                             farr_tra_det_atm_issste[arr_c].rfc_ent)
                    IF       g_ind_afi         >  0     THEN
                             LET     farr_tra_det_atm_issste[arr_c].n_seguro  =
                                     arr_afi[g_ind_afi].n_seguro_a
                             DISPLAY farr_tra_det_atm_issste[arr_c].n_seguro TO
                                     sr_ctr_folio1.n_seguro
                             UPDATE  tra_det_atm_issste
                                SET  n_seguro         =
                                     arr_afi[g_ind_afi].n_seguro_a
                              WHERE  n_seguro_ent   = 
                                     farr_tra_det_atm_issste[arr_c].n_seguro_ent
                                AND  folio_interno  =
                                    farr_tra_det_atm_issste[arr_c].folio_interno
                                AND  correlativo    =
                                     farr_tra_det_atm_issste[arr_c].correlativo
                                AND  estado         =
                                     farr_tra_det_atm_issste[arr_c].estado      
                                AND  tipo_criterio  =
                                    farr_tra_det_atm_issste[arr_c].criterio_cod
                    END IF

           END IF

     ON KEY(F3)
      LET arr_c = ARR_CURR()
      IF vtipo = 2  THEN
      LET b_uaep = 0

      IF vtipo = 2 THEN
	
        SELECT "OK"
        FROM   tra_det_atm_issste a
        WHERE  a.n_seguro       = farr_tra_det_atm_issste[arr_c].n_seguro 
        AND    a.n_seguro_ent   = farr_tra_det_atm_issste[arr_c].n_seguro_ent
        AND    a.rfc_ent        = farr_tra_det_atm_issste[arr_c].rfc_ent
        AND    a.cve_ced_cuenta = f_icefa_cod 
        AND    a.nro_ctrl_icefa = farr_tra_det_atm_issste[arr_c].nro_ctrl_icefa
        AND    a.nombre_ent[1,40]   = farr_tra_det_atm_issste[arr_c].paterno
        AND    a.nombre_ent[41,80]  = farr_tra_det_atm_issste[arr_c].materno
        AND    a.nombre_ent[81,120] = farr_tra_det_atm_issste[arr_c].nombres
        AND    (a.estado   IN (110,160,161,163,164,167,168) OR 
                a.estado   IN (SELECT G.estado FROM tra_status G))
        AND    a.correlativo <>   farr_tra_det_atm_issste[arr_c].correlativo
        GROUP BY 1

         IF STATUS <> NOTFOUND THEN

           UPDATE tra_det_atm_issste
           SET    tra_det_atm_issste.estado      = 163,
                  tra_det_atm_issste.fecha_edo   = TODAY,
                  tra_det_atm_issste.usuario     = usuario
           WHERE  tra_det_atm_issste.correlativo = 
                  farr_tra_det_atm_issste[arr_c].correlativo 

           LET v_mod = 163

         ELSE 

           INPUT BY NAME folio_uaep
              AFTER FIELD folio_uaep
		IF folio_uaep < 1000001 THEN 
		   ERROR"Folio UAEP fuera de rango"
		   NEXT FIELD folio_uaep
                END IF
              ON KEY (ESC)
		IF folio_uaep < 1000001 THEN 
		   ERROR"Folio UAEP fuera de rango"
		   NEXT FIELD folio_uaep
                END IF

		 SELECT "OK" 
		 FROM   tra_mae_icefa_issste a
		 WHERE  a.n_folio_tra = folio_uaep
		 GROUP BY 1
		
		 IF STATUS <> NOTFOUND THEN
		    ERROR "FOLIO UAEP YA EXISTE EN MAESTRO DE ICEFAS ISSSTE"
		    NEXT FIELD folio_uaep 
                 END IF
                 EXIT INPUT
	      ON KEY (INTERRUPT)
		 LET b_uaep = 1
		 LET v_mod = 112 
		 EXIT INPUT
              END INPUT

          IF b_uaep = 0 THEN 
            UPDATE tra_det_atm_issste
            SET    tra_det_atm_issste.estado      = 168,
                   tra_det_atm_issste.fecha_edo   = TODAY,
                   tra_det_atm_issste.usuario     = usuario
            WHERE  tra_det_atm_issste.correlativo = 
                   farr_tra_det_atm_issste[arr_c].correlativo 

             LET v_mod = 168


            LET ejecuta_p = 'fglgo TRAC077 ',farr_tra_det_atm_issste[arr_c].correlativo CLIPPED," ",folio_uaep CLIPPED 
            RUN ejecuta_p

     DISPLAY "TIPO CRITERIO:"," ",f_criterio_cod  CLIPPED," ",
                             f_criterio_desc CLIPPED AT 4,1 
     DISPLAY "ICEFA        :"," ",f_icefa_cod CLIPPED," ",f_cve_desc CLIPPED AT 5,1

     DISPLAY "de: ["," ",tot_reg_pant USING "######","]" AT 15, 20

     DISPLAY "<F1>Confirma <F2>Deshacer <F3>Confirma UAEP" AT 21,2 ATTRIBUTE(REVERSE)
           ERROR"REGISTRO INSERTADO EN EL MAESTRO DE ICEFA ISSSTE"

	  END IF 

          END IF

	END IF
	   
           INSERT INTO rep_aut 
           VALUES  (farr_tra_det_atm_issste[arr_c].correlativo)

           CALL trae_estado(v_mod)    

           RETURNING farr_tra_det_atm_issste[arr_c].estado       ,
                     farr_tra_det_atm_issste[arr_c].estado_desc

           DISPLAY BY NAME farr_tra_det_atm_issste[arr_c].estado ,
                           farr_tra_det_atm_issste[arr_c].estado_desc

           LET farr_tra_det_atm_issste[arr_c].usuario = usuario

           DISPLAY BY NAME farr_tra_det_atm_issste[arr_c].usuario
      END IF
     ON KEY(F1)
       LET arr_c = ARR_CURR()
        IF     farr_tra_det_atm_issste[arr_c].n_seguro     IS  NULL   OR
               farr_tra_det_atm_issste[arr_c].n_seguro      =  " "     THEN
               ERROR   "   CON  NSS AFORE  NULO  NO  PUEDE  CONFIRMAR      "
        ELSE
        
   

        SELECT "OK"
        FROM   tra_det_atm_issste  a
        WHERE  a.n_seguro       = farr_tra_det_atm_issste[arr_c].n_seguro 
        AND    a.n_seguro_ent   = farr_tra_det_atm_issste[arr_c].n_seguro_ent
        AND    a.rfc_ent        = farr_tra_det_atm_issste[arr_c].rfc_ent
        AND    a.cve_ced_cuenta = f_icefa_cod 
        AND    a.nro_ctrl_icefa = farr_tra_det_atm_issste[arr_c].nro_ctrl_icefa
        AND    a.nombre_ent[1,40]   = farr_tra_det_atm_issste[arr_c].paterno
        AND    a.nombre_ent[41,80]  = farr_tra_det_atm_issste[arr_c].materno
        AND    a.nombre_ent[81,120] = farr_tra_det_atm_issste[arr_c].nombres
        AND    a.estado   <> 0
        AND    a.correlativo <>   farr_tra_det_atm_issste[arr_c].correlativo
        GROUP BY 1

        IF STATUS = NOTFOUND THEN
         IF vtipo = 1 THEN 
           UPDATE tra_det_atm_issste
           SET    tra_det_atm_issste.estado = 160,
                  tra_det_atm_issste.fecha_edo = TODAY ,
                  tra_det_atm_issste.usuario   = usuario
           WHERE  tra_det_atm_issste.correlativo = 
                  farr_tra_det_atm_issste[arr_c].correlativo 
           LET v_mod = 160
         ELSE 
           UPDATE tra_det_atm_issste
           SET    tra_det_atm_issste.estado = 164,
                  tra_det_atm_issste.fecha_edo = TODAY,
                  tra_det_atm_issste.usuario   = usuario
           WHERE  tra_det_atm_issste.correlativo = 
                  farr_tra_det_atm_issste[arr_c].correlativo 
           LET v_mod = 164
         END IF

        ELSE

        SELECT "OK"
        FROM   tra_det_atm_issste a
        WHERE  a.n_seguro       = farr_tra_det_atm_issste[arr_c].n_seguro 
        AND    a.n_seguro_ent   = farr_tra_det_atm_issste[arr_c].n_seguro_ent
        AND    a.rfc_ent        = farr_tra_det_atm_issste[arr_c].rfc_ent
        AND    a.cve_ced_cuenta = f_icefa_cod 
        AND    a.nro_ctrl_icefa = farr_tra_det_atm_issste[arr_c].nro_ctrl_icefa
        AND    a.nombre_ent[1,40]   = farr_tra_det_atm_issste[arr_c].paterno
        AND    a.nombre_ent[41,80]  = farr_tra_det_atm_issste[arr_c].materno
        AND    a.nombre_ent[81,120] = farr_tra_det_atm_issste[arr_c].nombres
        AND    (a.estado   IN (110,160,161,163,164,167,168) OR 
                a.estado   IN (SELECT G.estado FROM tra_status G))
        AND    a.correlativo <>   farr_tra_det_atm_issste[arr_c].correlativo
        GROUP BY 1

         IF STATUS <> NOTFOUND THEN

           UPDATE tra_det_atm_issste
           SET    tra_det_atm_issste.estado = 163,
                  tra_det_atm_issste.fecha_edo = TODAY,
                  tra_det_atm_issste.usuario   = usuario
           WHERE  tra_det_atm_issste.correlativo = 
                  farr_tra_det_atm_issste[arr_c].correlativo 

           LET v_mod = 163

         ELSE 
          IF vtipo = 1 THEN
           UPDATE tra_det_atm_issste
           SET    tra_det_atm_issste.estado = 160,
                  tra_det_atm_issste.fecha_edo = TODAY,
                  tra_det_atm_issste.usuario = usuario
           WHERE  tra_det_atm_issste.correlativo = 
                  farr_tra_det_atm_issste[arr_c].correlativo 
           LET v_mod = 160
          ELSE 
           UPDATE tra_det_atm_issste
           SET    tra_det_atm_issste.estado = 164,
                  tra_det_atm_issste.fecha_edo = TODAY,
                  tra_det_atm_issste.usuario   = usuario
           WHERE  tra_det_atm_issste.correlativo = 
                  farr_tra_det_atm_issste[arr_c].correlativo 
           LET v_mod = 164
          END IF
         END IF
       END IF

           INSERT INTO rep_aut 
           VALUES  (farr_tra_det_atm_issste[arr_c].correlativo)

           CALL trae_estado(v_mod)    

           RETURNING farr_tra_det_atm_issste[arr_c].estado       ,
                     farr_tra_det_atm_issste[arr_c].estado_desc

           DISPLAY BY NAME farr_tra_det_atm_issste[arr_c].estado ,
                           farr_tra_det_atm_issste[arr_c].estado_desc

           LET farr_tra_det_atm_issste[arr_c].usuario = usuario

           DISPLAY BY NAME farr_tra_det_atm_issste[arr_c].usuario
       END IF
     ON KEY(F2)

       LET arr_c = ARR_CURR() 

       IF (farr_tra_det_atm_issste[arr_c].estado = 110 OR 
           farr_tra_det_atm_issste[arr_c].estado = 112 ) THEN
         LET v_mod = vestado
       ELSE 
         IF vestado = 110 THEN
           UPDATE tra_det_atm_issste
           SET    tra_det_atm_issste.estado = 110 
           WHERE  tra_det_atm_issste.correlativo = 
                  farr_tra_det_atm_issste[arr_c].correlativo 
   
          LET v_mod = 110
         ELSE 
           UPDATE tra_det_atm_issste
           SET    tra_det_atm_issste.estado = 112
           WHERE  tra_det_atm_issste.correlativo = 
                  farr_tra_det_atm_issste[arr_c].correlativo 
   
          LET v_mod = 112
         END IF
      END IF

             IF farr_tra_det_atm_issste[arr_c].estado = 168 THEN
		    DELETE FROM tra_mae_icefa_issste
                    WHERE  correlativo =  farr_tra_det_atm_issste[arr_c].correlativo
	     END IF


           INSERT INTO rep_aut 
           VALUES  (farr_tra_det_atm_issste[arr_c].correlativo)
           CALL trae_estado(v_mod)    
           RETURNING farr_tra_det_atm_issste[arr_c].estado,
                     farr_tra_det_atm_issste[arr_c].estado_desc
           DISPLAY BY NAME farr_tra_det_atm_issste[arr_c].estado,
                           farr_tra_det_atm_issste[arr_c].estado_desc

     ON KEY(INTERRUPT)
        EXIT DISPLAY
     END DISPLAY
     END WHILE
END FUNCTION

FUNCTION    F_100_despliega_afiliados(l_rfc)
   DEFINE   l_rfc              LIKE     afi_mae_afiliado.n_rfc
   DEFINE   l_rfc_10                           CHAR(10)
   DEFINE   ind                                ,
            l_tot_reg                          ,
            l_arr_count                        ,
            l_scr_line                         ,
            l_arr_curr                         SMALLINT
   LET      l_tot_reg                =  0
   LET      ind                      =  1
   LET      l_rfc_10                 =  l_rfc   [1,10]
   DECLARE  c_afiliados      CURSOR   FOR
            SELECT   C.n_seguro,C.n_rfc,C.paterno,C.materno,C.nombres
              FROM   safre_af:afi_mae_afiliado  C
             WHERE   C.n_rfc[1,10]     =  l_rfc_10
#            WHERE   C.n_rfc    =  l_rfc
   FOREACH  c_afiliados       INTO      arr_afi[ind].*
            LET      ind             =  ind     +       1
   END FOREACH
   LET      ind                      =  ind  -  1
   LET      g_ind_afi                =  0
   CALL     SET_COUNT(ind)
   DISPLAY  ARRAY    arr_afi        TO  sr_ctr_afi.*
            ON KEY  (RETURN )
                     IF       ind           >       0    THEN
                              LET      g_ind_afi             =  ARR_CURR()
                     END IF
                     EXIT     DISPLAY
            ON KEY (ESC)
                     EXIT     DISPLAY
   END DISPLAY
END FUNCTION

FUNCTION despliega_consulta (f_folio,f_cve_desc,f_criterio_cod,
                             f_criterio_desc)
#dc------------------------------------------------------------

DEFINE f_folio          INTEGER
DEFINE f_cve_desc       CHAR(10)
DEFINE f_criterio_cod   SMALLINT 
DEFINE f_criterio_desc  CHAR(030)
DEFINE f_correlativo    INTEGER
DEFINE f_icefa_cod      CHAR(05) 

DEFINE farr_tra_det_atm_issste ARRAY[10000] OF RECORD 
       criterio_cod   LIKE tra_tab_valcri.criterio_cod,
       criterio_desc   char(030),
       folio_interno  LIKE tra_det_atm_issste.folio_interno,
       icefa_cod      LIKE tab_icefa.icefa_cod,
       icefa_desc     LIKE tab_icefa.icefa_desc,
       correlativo    LIKE tra_det_atm_issste.correlativo,
       n_seguro       LIKE tra_det_atm_issste.n_seguro,
       n_seguro_ent   LIKE tra_det_atm_issste.n_seguro_ent,
       rfc_ent        LIKE tra_det_atm_issste.rfc_ent,
       usuario        CHAR(010)                            ,
       nro_ctrl_icefa LIKE tra_det_atm_issste.nro_ctrl_icefa,
       folio_uaep     INTEGER   ,
       paterno        CHAR(040) ,
       materno        CHAR(040) ,
       nombres        CHAR(040) ,
       sar_92_issste   LIKE tra_det_atm_issste.sar_92_issste ,  
       viv_92_issste   LIKE tra_det_atm_issste.viv_92_issste ,
       estado         LIKE tra_det_atm_issste.estado       ,
       estado_desc    CHAR(040)                            ,
       num_lin        INTEGER                              ,
       n_seguro_a     like afi_mae_afiliado.n_seguro       ,
       n_rfc_a        like afi_mae_afiliado.n_rfc          ,
       paterno_a      like afi_mae_afiliado.paterno        ,
       materno_a      like afi_mae_afiliado.materno        ,
       nombres_a      like afi_mae_afiliado.nombres
END RECORD

DEFINE freg_tra_det_atm_issste         RECORD 
       criterio_cod   LIKE tra_tab_valcri.criterio_cod,
       criterio_desc   char(030),
       folio_interno  LIKE tra_det_atm_issste.folio_interno,
       icefa_cod      LIKE tab_icefa.icefa_cod,
       icefa_desc     LIKE tab_icefa.icefa_desc,
       correlativo    LIKE tra_det_atm_issste.correlativo,
       n_seguro       LIKE tra_det_atm_issste.n_seguro,
       n_seguro_ent   LIKE tra_det_atm_issste.n_seguro_ent,
       rfc_ent        LIKE tra_det_atm_issste.rfc_ent,
       usuario        CHAR(010) ,
       nro_ctrl_icefa LIKE tra_det_atm_issste.nro_ctrl_icefa,
       folio_uaep     INTEGER   ,
       paterno        CHAR(040) ,
       materno        CHAR(040) ,
       nombres        CHAR(040) ,
       sar_92_issste   LIKE tra_det_atm_issste.sar_92_issste ,
       viv_92_issste   LIKE tra_det_atm_issste.viv_92_issste ,
       estado         SMALLINT  ,
       estado_desc    CHAR(040) ,
       num_lin        INTEGER                         ,
       n_seguro_a     like afi_mae_afiliado.n_seguro       ,
       n_rfc_a        like afi_mae_afiliado.n_rfc          ,
       paterno_a      like afi_mae_afiliado.paterno        ,
       materno_a      like afi_mae_afiliado.materno        ,
       nombres_a      like afi_mae_afiliado.nombres
END RECORD

DEFINE festado,
       arr_c                ,
       i             SMALLINT,
       tot_reg_pant  SMALLINT 

LET ffb = 0

WHILE TRUE
  CLEAR FORM
  CONSTRUCT BY NAME x_busca ON A.n_seguro   

        ON KEY (ESC )
            LET INT_FLAG = FALSE
            EXIT CONSTRUCT

        ON KEY (INTERRUPT )
            LET INT_FLAG = FALSE
	    LET ffb = 1
            EXIT CONSTRUCT
    END CONSTRUCT

   IF ffb = 1 THEN
       LET ffb = 0
       EXIT WHILE
   END IF

     SELECT A.icefa_cod
        INTO f_icefa_cod
        FROM tab_icefa A
     WHERE f_cve_desc = A.icefa_desc  
     GROUP BY 1


     SELECT MIN(A.correlativo)
     INTO   f_correlativo
     FROM   tra_det_atm_issste A
     WHERE  A.folio_interno = f_folio
--     AND    A.estado        = vestado
 
     IF f_correlativo IS NULL THEN 
        ERROR "NO HAY REGISTROS PARA MOSTRAR "
        SLEEP 3
        RETURN
     END IF


LET txt =       
    'SELECT A.cve_ced_cuenta     ,    ',
    ' A.tipo_criterio            ,    ',
    ' A.folio_interno,                ',
    ' A.correlativo,                  ',
    ' A.n_seguro,                     ',
    ' A.n_seguro_ent ,                ',
    ' A.rfc_ent ,                     ',
    ' A.usuario           ,           ',
    ' A.nro_ctrl_icefa ,              ',
    ' A.nombre_ent[001,040],    ',
    ' A.nombre_ent[041,080],    ',
    ' A.nombre_ent[081,120],    ',
    ' A.sar_92_issste      ,    ',
    ' A.viv_92_issste      ,    ',
    ' A.estado            ,     ',
    ' Z.estado_descripcion      ',
    'FROM   tra_det_atm_issste A, ',
    '       tra_aut_estado     Z ',
    ' WHERE  ',x_busca CLIPPED,
    ' AND    A.folio_interno =  ',f_folio       ,
    ' AND    A.correlativo   >= ',f_correlativo ,
    ' AND    A.estado        = Z.estado_cod    ',
    ' ORDER BY 2,A.correlativo'
     LET txt = txt CLIPPED
 
     PREPARE qry2 FROM txt

     DECLARE cur_dsply_consulta CURSOR FOR qry2

     LET i = 1
     LET tot_reg_pant   = 0 
     LET freg_tra_det_atm_issste.num_lin       = 1

     FOREACH cur_dsply_consulta

     INTO freg_tra_det_atm_issste.icefa_cod    ,
          freg_tra_det_atm_issste.criterio_cod    ,
          freg_tra_det_atm_issste.folio_interno    ,
          freg_tra_det_atm_issste.correlativo      ,
          freg_tra_det_atm_issste.n_seguro         ,
          freg_tra_det_atm_issste.n_seguro_ent     ,
          freg_tra_det_atm_issste.rfc_ent          ,
          freg_tra_det_atm_issste.usuario          ,
          freg_tra_det_atm_issste.nro_ctrl_icefa   ,
          freg_tra_det_atm_issste.paterno          ,
          freg_tra_det_atm_issste.materno          ,
          freg_tra_det_atm_issste.nombres          ,
          freg_tra_det_atm_issste.sar_92_issste     ,
          freg_tra_det_atm_issste.viv_92_issste     ,
          freg_tra_det_atm_issste.estado           ,
          freg_tra_det_atm_issste.estado_desc

      IF    vtipo          =  1      THEN 
          SELECT   C.n_seguro,C.n_rfc,C.paterno,C.materno,C.nombres
            INTO   freg_tra_det_atm_issste.n_seguro_a       ,
                   freg_tra_det_atm_issste.n_rfc_a          ,
                   freg_tra_det_atm_issste.paterno_a        ,
                   freg_tra_det_atm_issste.materno_a        ,
                   freg_tra_det_atm_issste.nombres_a
            FROM   safre_af:afi_mae_afiliado  C
           WHERE   C.n_seguro      =  freg_tra_det_atm_issste.n_seguro;
      END IF
          IF freg_tra_det_atm_issste.estado = 168 THEN
	     SELECT a.n_folio_tra 
	     INTO  freg_tra_det_atm_issste.folio_uaep
	     FROM tra_mae_icefa_issste a 
	     WHERE a.correlativo = freg_tra_det_atm_issste.correlativo
          ELSE 
	     LET freg_tra_det_atm_issste.folio_uaep = null
          END IF

          SELECT icefa_desc 
	  INTO   freg_tra_det_atm_issste.icefa_desc  
	  FROM   tab_icefa 
	  WHERE  icefa_cod = freg_tra_det_atm_issste.icefa_cod

          SELECT vector
	  INTO   freg_tra_det_atm_issste.criterio_desc
	  FROM   tra_tab_valcri 
	  WHERE  criterio_cod = freg_tra_det_atm_issste.criterio_cod


          LET  farr_tra_det_atm_issste[i].* =
               freg_tra_det_atm_issste.*

          LET freg_tra_det_atm_issste.num_lin = 
	      freg_tra_det_atm_issste.num_lin + 1 

          LET i = i + 1
          LET tot_reg_pant = tot_reg_pant + 1
         
                  
          IF i >= 9999 THEN  
             EXIT FOREACH
          END IF
     
     END FOREACH
     

     CALL SET_COUNT(i-1)
    
     DISPLAY "de: ["," ",tot_reg_pant USING "######","]" AT 15, 20

     DISPLAY ARRAY farr_tra_det_atm_issste
                TO sr_ctr_folio2.*

     ON  KEY(RETURN)
           IF       vtipo          =  2      THEN
                    CALL     F_100_despliega_afiliados (
                    freg_tra_det_atm_issste.rfc_ent   )
           END IF
     ON KEY(INTERRUPT)
        EXIT DISPLAY
     END DISPLAY
     END WHILE
END FUNCTION

FUNCTION init() 
#i------------- 

LET HOY                               =                                 TODAY 
LET HORA                              =                                 TIME
LET g_nom_prog                        =                                "TRAM071"

    CREATE TEMP TABLE rep_aut(correlativo integer)
     
    SELECT *
    INTO   reg_param_tra.*
    FROM  seg_modulo
    WHERE modulo_cod = "tra"

    SELECT user
    INTO   usuario
    FROM   tab_afore_local

    LET ruta = reg_param_tra.ruta_listados       CLIPPED,
    	       "/",usuario CLIPPED ,".TRAM071." ,
	       HOY USING "YYYYMMDD"    CLIPPED,".",
               HORA CLIPPED

    SELECT codigo_afore,
           razon_social
    INTO g_glob.*
    FROM safre_af:tab_afore_local


END FUNCTION

REPORT listado_1(reg_tra_det_atm_issste)
#l1--------------------------------------

DEFINE reg_tra_det_atm_issste RECORD LIKE tra_det_atm_issste.*
DEFINE i_cont_reg_total integer
DEFINE i_cont_reg_orig integer

    OUTPUT
        PAGE LENGTH 90

    FORMAT

    PAGE HEADER
IF sw = 0 
THEN LET i_cont_reg_total = 0
LET i_cont_reg_orig = 0
LET sw = 1
END IF
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 001,"====================================================",
                       "====================================================",
                       "====================================================",
                       "=================" 
        PRINT
            COLUMN 001,g_glob.codigo_afore USING "###","  ",g_glob.razon_social CLIPPED
        PRINT
            COLUMN 001,HOY USING"DD-MM-YYYY" ,
            COLUMN 155,g_nom_prog CLIPPED
        PRINT
            COLUMN 001,HORA,
            COLUMN 050,"REPORTE DE VALIDACION MANUAL CRUCE SAR 92 TRAS",
                       "PASO ICEFA-AFORE ISSSTE"
       PRINT
            COLUMN 001,"NSS"         ,
            COLUMN 013,"NSS ICEFA"   ,
            COLUMN 025,"RFC ICEFA"   ,
            COLUMN 040,"BANCO"       ,
            COLUMN 049,"NRO.INTERNO" 
            PRINT
            COLUMN 001,"NOMBRE"      
       PRINT
            COLUMN 001,"====================================================",
                       "====================================================",
                       "====================================================",
                       "=================" 
        PRINT

    BEFORE GROUP OF reg_tra_det_atm_issste.estado
        LET i_cont_reg_orig = 0
        PRINT
        PRINT
       PRINT
            COLUMN 001,"----------------------------------------------------",
                       "----------------------------------------------------",
                       "----------------------------------------------------",
                       "-----------------" 
        PRINT
        PRINT
            COLUMN 001,"ULTIMO ESTADO :",
                       reg_tra_det_atm_issste.estado 
        PRINT
        PRINT
    ON EVERY ROW
        LET i_cont_reg_orig  = i_cont_reg_orig + 1
       -- LET i_cont_reg_total = i_cont_reg_total + 1
        PRINT
            COLUMN 001,reg_tra_det_atm_issste.n_seguro             ,
            COLUMN 013,reg_tra_det_atm_issste.n_seguro_ent         ,
            COLUMN 025,reg_tra_det_atm_issste.rfc_ent              ,
            COLUMN 040,reg_tra_det_atm_issste.cve_ced_cuenta       ,
            COLUMN 049,reg_tra_det_atm_issste.nro_ctrl_icefa       
            PRINT
            COLUMN 001,reg_tra_det_atm_issste.nombre_ent[1,40]     ,
            COLUMN 042,reg_tra_det_atm_issste.nombre_ent[41,80]    ,
            COLUMN 084,reg_tra_det_atm_issste.nombre_ent[81,120]
        PRINT
            
    AFTER GROUP OF reg_tra_det_atm_issste.estado
       -- LET i_cont_reg_total = i_cont_reg_total + i_cont_reg_orig
        PRINT
        PRINT
            COLUMN 001,"TOTAL DE REGISTROS :",reg_tra_det_atm_issste.estado,
                       " :          ",i_cont_reg_orig USING"#########"
        PRINT 	

    ON LAST ROW

         SELECT COUNT(*)
         INTO i_cont_reg_total
         FROM  tra_det_atm_issste A
         WHERE  A.correlativo IN 
               (SELECT unique H.correlativo
                FROM   rep_aut H)
         AND    A.estado IN (160)

        PRINT	
            COLUMN 001,"====================================================",
                       "====================================================",
                       "====================================================",
                       "=================" 
        PRINT
            COLUMN 001,"TOTAL DE REGISTROS MODIFICADOS MANUALMENTE :       ",
                       i_cont_reg_total USING"#########"
        PRINT      
            COLUMN 001,"====================================================",
                       "====================================================",
                       "====================================================",
                       "=================" 
      
                                                       
END REPORT
FUNCTION trae_estado(vestado_cod)
#te-----------------------------


DEFINE vestado_cod SMALLINT
DEFINE v_estado_desc  CHAR(040)

     SELECT a.estado_descripcion
     INTO v_estado_desc
     FROM  tra_aut_estado a
     WHERE a.estado_cod = vestado_cod      


RETURN vestado_cod,v_estado_desc

END FUNCTION

{FUNCTION uno()
#u------------
DEFINE cad CHAR(300)

LET cad = "fglgo TRAM072"
RUN cad

END FUNCTION
}

FUNCTION consulta_estados()
#u------------
DEFINE cad CHAR(300)

LET cad = "fglgo TRAM073"
RUN cad

END FUNCTION


