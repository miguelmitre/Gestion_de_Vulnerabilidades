#-- AUTOR                 :  MARCO ANTONIO GONZALEZ ROJAS --#
#-- FECHA DE MODIFICACION :  07OCT 2010

#--ESTB101.4gl--#
DATABASE safre_af
GLOBALS 

define checa       DECIMAL(16,6)
DEFINE pre_acc_1                          ,
       pre_acc_2                          ,
       pre_acc_3                          ,
       pre_acc_4                          ,
       pre_acc_5                          ,
       pre_acc_6                          ,
       pre_acc_7              DECIMAL(16,6)

DEFINE l_acc decimal(16,6)
DEFINE reg_modulo RECORD LIKE seg_modulo.*
DEFINE ruta char(1000)

DEFINE reg_1 ARRAY[24] OF RECORD 
             tot_x_elec                      ,
             tot_x_proc_oper                 ,
             tot_elecmasoper          INTEGER,
             rcv_imss                        ,
             seg_ret                         ,
             rcv_issste                      ,
             ahorro_solid                    ,
             ahorro_ret                      ,
             apor_vol                        ,
             compl_ret                       ,
             d_ahorro_larg_pla               ,
             c_persp_larg_pla                ,                
             tot_sie_niv         DECIMAL(16,6)
                          END RECORD


DEFINE  reg_3  RECORD
        nivel              SMALLINT ,
        tip_col_mto        SMALLINT ,
        monto_en_acciones  DECIMAL(16,6)
               END RECORD

DEFINE v_ind_edad SMALLINT

DEFINE fecha_ini date ,
       fecha_fin date


DEFINE v_existe           ,
       v_criterio SMALLINT


DEFINE i SMALLINT 
DEFINE txt                 , 
       sql_texto  CHAR(4000)

DEFINE g_nivel    SMALLINT
DEFINE g_uni_nss  CHAR(11)

END GLOBALS
       
MAIN   
       
   LET fecha_ini                  =               ARG_VAL(1)
   LET fecha_fin                  =               ARG_VAL(2)

     IF fecha_fin < fecha_ini THEN
        DISPLAY "FECHA FIN NO PUEDE SER MENOR A FECHA INI..."
        DISPLAY "ABORTANDO PROCESO..."
        EXIT PROGRAM
     END IF

     IF fecha_fin IS NULL THEN
        DISPLAY "FECHA FIN NO PUEDE SER NULA..."
        DISPLAY "ABORTANDO PROCESO..."
        EXIT PROGRAM
     END IF

     IF MONTH(fecha_ini)  <>  MONTH(fecha_fin)  THEN
        DISPLAY "EL MES DE FECHA INI Y FECHA FIN DEBEN SER IGUALES..."
        DISPLAY "ABORTANDO PROCESO..."
        EXIT PROGRAM 
     END IF

   CALL inicio()
   CALL limpia_var()

   LET  i       =       0

   DECLARE cur_1 CURSOR FOR 


      SELECT A.nivel              ,
             A.tip_col_mto        ,
             A.monto_en_acciones
      FROM  formatob   A
      ORDER BY 1,2

   FOREACH cur_1  INTO reg_3.nivel            ,
                       reg_3.tip_col_mto      ,
                       reg_3.monto_en_acciones 
                             
  
      LET i     =      reg_3.nivel

      CASE reg_3.tip_col_mto
         WHEN  1 
            LET reg_1[i].rcv_imss          = reg_1[i].rcv_imss + reg_3.monto_en_acciones
         WHEN  2
            LET reg_1[i].seg_ret           = reg_1[i].seg_ret  + reg_3.monto_en_acciones 
	     WHEN  3
            LET reg_1[i].rcv_issste        = reg_1[i].rcv_issste + reg_3.monto_en_acciones
 	     WHEN  4
            LET reg_1[i].ahorro_solid      = reg_1[i].ahorro_solid  + reg_3.monto_en_acciones
         WHEN  5
            LET reg_1[i].ahorro_ret        = reg_1[i].ahorro_ret + reg_3.monto_en_acciones
         WHEN  6
            LET reg_1[i].apor_vol          = reg_1[i].apor_vol  + reg_3.monto_en_acciones
	     WHEN  7
            LET reg_1[i].compl_ret         = reg_1[i].compl_ret + reg_3.monto_en_acciones
	     WHEN  8
            LET reg_1[i].d_ahorro_larg_pla = reg_1[i].d_ahorro_larg_pla + reg_3.monto_en_acciones 			   
	    WHEN  9
            LET reg_1[i].c_persp_larg_pla  = reg_1[i].c_persp_larg_pla + reg_3.monto_en_acciones 			   

      END CASE

      LET reg_1[i].tot_sie_niv = reg_1[i].tot_sie_niv  + reg_3.monto_en_acciones


   END FOREACH 

  
#----------------                       --------    
#-- Totales por NIVEL y POR ELECCION    --------

   LET g_nivel            =     0
   LET g_uni_nss          =     0
      
   DECLARE d CURSOR FOR 

      SELECT  a.nivel,COUNT(UNIQUE a.nss )
      FROM    formatob a 
      WHERE   a.tipo_traspaso IN ( 1,9 )
      GROUP BY 1
      ORDER BY 1

   FOREACH d INTO g_nivel   ,
                  g_uni_nss

      LET reg_1[g_nivel].tot_x_elec       =   g_uni_nss     
      
      

   END FOREACH 


#----------------                         --------    
#-- Totales por NIVEL PROCESO OPERATIVO   --------

   LET g_nivel            =     0
   LET g_uni_nss          =     0
      
   DECLARE d1 CURSOR FOR 

      SELECT  a.nivel,COUNT(UNIQUE a.nss )
      FROM    formatob a 
      WHERE   a.tipo_traspaso IN ( 0,2,3,4,5,6,7,8,10,11,12,13,14,15,16 )
      GROUP BY 1
      ORDER BY 1

   FOREACH d1 INTO g_nivel   ,
                   g_uni_nss

      LET reg_1[g_nivel].tot_x_proc_oper   =   g_uni_nss                                      
      
      

   END FOREACH 

{
#----------------                       ------------------
 
#----------------                                              -------- 
#-- < Totales por NIVEL >  POR ELECCION  +   PROCESO OPERATIVO -------- 


   LET g_nivel            =     0                                    
   LET g_uni_nss          =     0                                    
                                                                     
   DECLARE d2 CURSOR FOR                                             
                                                                     
     #SELECT  a.nivel,COUNT(UNIQUE a.nss )                           
      SELECT  a.nivel,COUNT(nss)                           
      FROM    formatob a                                             
      WHERE   a.tipo_traspaso IN ( 0,1,2,3,4,5,6,7,8,9,10,11,13,14,15,16 )  
      GROUP BY 1                                                     
      ORDER BY 1                                                     
                                                                     
   FOREACH d2 INTO g_nivel   ,                                       
                   g_uni_nss                                         
                                                                     
      LET reg_1[g_nivel].tot_elecmasoper   =   g_uni_nss             
                                                                     
   END FOREACH                                                       

}

#----------------                       ------------------
 
   LET ruta = reg_modulo.ruta_listados CLIPPED ,"/formatob_",fecha_ini USING"YYYYMMDD","-",
                      fecha_fin USING"YYYYMMDD"

   START REPORT  rpt_1 TO ruta

      OUTPUT TO REPORT rpt_1()

   FINISH REPORT rpt_1

   UPDATE safre_af:taa_ctr_anexob
   SET    estado = "FIN"

END MAIN


REPORT rpt_1()
#r1----------------

 DEFINE campo      RECORD
        afore_cod  CHAR(03),
        raz_social CHAR(50)
                   END  RECORD

 DEFINE transf     CHAR(10)

 DEFINE r_nivel                ,
        y                      ,
        sie                    ,
        r_tipo_col_mto  SMALLINT

 DEFINE reg_tot ARRAY[24] OF DECIMAL(16,6)
 DEFINE rep_arr ARRAY[7]  OF INTEGER           

 DEFINE r_tot_solic        ,
        tot_tra_s    INTEGER
        
 DEFINE                                
        r_tot_rec_sb1          ,       
        r_tot_rec_sb2          ,       
        r_tot_rec_sb3          ,       
        r_tot_rec_sb4          ,       
        r_tot_rec_sb5          ,       
        r_tot_rec_sb6          ,       
        r_tot_rec_sb7          ,       
        r_tot_rec_s1           ,       
        r_tot_rec_s2           ,       
        r_tot_rec_s3           ,       
        r_tot_rec_s4           ,       
        r_tot_rec_s5           ,       
        r_tot_rec_s6           ,              
        r_tot_rec_s7           ,
        r_SB1                  ,
        r_SB2                  ,  
        r_SB3                  ,
        r_SB4                  ,
        r_SB5                  ,
        r_SB6                  ,
        r_SB7            INTEGER
        

 OUTPUT
        TOP MARGIN     1
        BOTTOM MARGIN  0
        LEFT MARGIN    0
        RIGHT MARGIN   0
        PAGE LENGTH   60

   FORMAT
        PAGE HEADER
          PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
          PRINT COLUMN  108,"ANEXO B"
          PRINT COLUMN   88,"Flujo de Transferencias Voluntarias entre SIEFORE" 
              --COLUMN  165,"Pagina: ",PAGENO USING "<<<<"
          SKIP 2 LINE
          PRINT COLUMN  02,"MES QUE REPORTA: ",MONTH(fecha_ini) USING "&&","-",YEAR(fecha_ini) USING "&&&&"

             SELECT codigo_afore,razon_social
             INTO   campo.afore_cod,campo.raz_social
             FROM   tab_afore_local

          PRINT COLUMN  02,"CLAVE AFORE: ",campo.afore_cod USING  "&&&"  
          PRINT COLUMN  02,"AFORE: ",campo.raz_social CLIPPED
          SKIP 1 LINE
          PRINT COLUMN 02,"__________________________________________________",
                          "__________________________________________________",
                          "__________________________________________________",
                          "__________________________________________________",
                          "______________________"

          
          #Primera Linea de titulos#
          PRINT COLUMN 115,"Numero de titulos" 
          
          #Segunda Linea de titulos#
          
          PRINT COLUMN  31,"Movimientos",
                COLUMN 156,"Ahorro Voluntario"
         
          #Tercera Linea de titulos#
          
          PRINT COLUMN  02,"Transferencias"          ,     
                COLUMN  31,"Por Procesos"           ,     
                COLUMN  60,"RCV IMSS"               ,   
                COLUMN  74,"Seguro del"             ,                                                                    
                COLUMN  90,"RCV ISSSTE"             ,        
                COLUMN 107,"Ahorro"       ,  
                COLUMN 124,"Ahorro para el"         ,    
                COLUMN 141,"Aportaciones"           ,     
                COLUMN 158,"Complementarias"        ,  
                COLUMN 176,"Ahorro Largo"            ,      
                COLUMN 191,"Perspect. Inv"        ,  
                COLUMN 213,"Total"     
                        

          PRINT COLUMN   17,"Por Eleccion"           ,       
                COLUMN   31,"Operativos"             ,                        
                COLUMN   45,"Total"                  ,                                        
                COLUMN   74,"Retiro"                 ,            
                COLUMN  107,"Solidario"              ,           
                COLUMN  125,"Retiro"                 ,           
                COLUMN  141,"Voluntarias"            ,      
                COLUMN  161,"de Retiro"              ,        
                COLUMN  179,"Plazo"            ,      
                COLUMN  191,"Largo Plazo"      ,
                COLUMN  212,"Siefore"           

          
          PRINT COLUMN 02,"__________________________________________________", 
                          "__________________________________________________", 
                          "__________________________________________________", 
                          "__________________________________________________", 
                          "______________________"                              
                
         

        SKIP 1 LINE

        ON EVERY ROW

           FOR r_nivel  =  1  TO  24

              CASE r_nivel

                 WHEN 1
                    LET transf  = "SB1 a SB2"
                 WHEN 2
                    LET transf  = "SB1 a SB3"
                 WHEN 3
                    LET transf  = "SB1 a SB4"
                 WHEN 4
                    LET transf  = "SB1 a SB5"
                 WHEN 5
                    LET transf  = "SB2 a SB1"
                 WHEN 6
                    LET transf  = "SB2 a SB3"
                 WHEN 7
                    LET transf  = "SB2 a SB4"
                 WHEN 8
                    LET transf  = "SB2 a SB5"
                 WHEN 9
                    LET transf  = "SB3 a SB1"
                 WHEN 10
                    LET transf  = "SB3 a SB2"
                 WHEN 11   
                    LET transf  = "SB3 a SB4"
                 WHEN 12   
                    LET transf  = "SB3 a SB5"
                 WHEN 13   
                    LET transf  = "SB4 a SB1"
                 WHEN 14   
                    LET transf  = "SB4 a SB2"
                 WHEN 15   
                    LET transf  = "SB4 a SB3"
                 WHEN 16   
                    LET transf  = "SB4 a SB5"
                 WHEN 17   
                    LET transf  = "SB5 a SB1"
                 WHEN 18   
                    LET transf  = "SB5 a SB2"
                 WHEN 19   
                    LET transf  = "SB5 a SB3"
                 WHEN 20   
                    LET transf  = "SB5 a SB4"
                 WHEN 21   
                    LET transf  = "SB a SACP"
                 WHEN 22   
                    LET transf  = "SB a SALP"
                 WHEN 23   
                    LET transf  = "SACP a SB"
                 WHEN 24   
                    LET transf  = "SALP a SB"

              END CASE


              LET reg_1[r_nivel].tot_elecmasoper  =  
                  reg_1[r_nivel].tot_x_elec +
                  reg_1[r_nivel].tot_x_proc_oper
                          
              
              PRINT COLUMN  02,transf  CLIPPED,                          
                    COLUMN  17,reg_1[r_nivel].tot_x_elec        USING "######&"         ,
                    COLUMN  31,reg_1[r_nivel].tot_x_proc_oper   USING "######&"         ,
                    COLUMN  45,reg_1[r_nivel].tot_elecmasoper   USING "######&"         ,
                   # -----  
                    COLUMN  53,reg_1[r_nivel].rcv_imss          USING "#########&.&&&&&&",
                    COLUMN  71,reg_1[r_nivel].seg_ret           USING "#########&.&&&&&&",
                    COLUMN  88,reg_1[r_nivel].rcv_issste        USING "#########&.&&&&&&",
                    COLUMN 105,reg_1[r_nivel].ahorro_solid      USING "#########&.&&&&&&",
                    COLUMN 122,reg_1[r_nivel].ahorro_ret        USING "#########&.&&&&&&",
                    COLUMN 139,reg_1[r_nivel].apor_vol          USING "#########&.&&&&&&",
                    COLUMN 156,reg_1[r_nivel].compl_ret         USING "#########&.&&&&&&",
                    COLUMN 173,reg_1[r_nivel].d_ahorro_larg_pla USING "#########&.&&&&&&",
                    COLUMN 190,reg_1[r_nivel].c_persp_larg_pla  USING "#########&.&&&&&&",
                    COLUMN 207,reg_1[r_nivel].tot_sie_niv       USING "#########&.&&&&&&"
                              
            END FOR

        SKIP 1 LINE

           PRINT COLUMN 02,"__________________________________________________", 
                           "__________________________________________________", 
                           "__________________________________________________", 
                           "__________________________________________________", 
                           "______________________"                         
        

        SKIP 1 LINE

           FOR  y                 =   1  TO  24
            
              LET reg_tot[y]      =   0

           END FOR

           FOR r_nivel  =  1  TO  24
              
              LET reg_tot[1]  = reg_tot[1]  +  reg_1[r_nivel].tot_x_elec
              LET reg_tot[2]  = reg_tot[2]  +  reg_1[r_nivel].tot_x_proc_oper
              LET reg_tot[3]  = reg_tot[3]  +  reg_1[r_nivel].tot_elecmasoper
              #------------                                  ------#
              LET reg_tot[4]  = reg_tot[4]  +  reg_1[r_nivel].rcv_imss   
              LET reg_tot[5]  = reg_tot[5]  +  reg_1[r_nivel].seg_ret
              LET reg_tot[6]  = reg_tot[6]  +  reg_1[r_nivel].rcv_issste
              LET reg_tot[7]  = reg_tot[7]  +  reg_1[r_nivel].ahorro_solid
              LET reg_tot[8]  = reg_tot[8]  +  reg_1[r_nivel].ahorro_ret   
              LET reg_tot[9]  = reg_tot[9]  +  reg_1[r_nivel].apor_vol    
              LET reg_tot[10] = reg_tot[10] +  reg_1[r_nivel].compl_ret    
              LET reg_tot[11] = reg_tot[11] +  reg_1[r_nivel].d_ahorro_larg_pla
              LET reg_tot[12] = reg_tot[12] +  reg_1[r_nivel].c_persp_larg_pla
              LET reg_tot[13] = reg_tot[13] +  reg_1[r_nivel].tot_sie_niv 

           END FOR       

                PRINT COLUMN  02,"Total Afore",                        
                      COLUMN  17,reg_tot[1]  USING  "######&"             ,
                      COLUMN  31,reg_tot[2]  USING  "######&"             ,
                      COLUMN  45,reg_tot[3]  USING  "######&"             ,
                      COLUMN  53,reg_tot[4]  USING  "#########&.&&&&&&"  ,
                      COLUMN  71,reg_tot[5]  USING  "#########&.&&&&&&"  ,
                      COLUMN  88,reg_tot[6]  USING  "#########&.&&&&&&"  ,
                      COLUMN 105,reg_tot[7]  USING  "#########&.&&&&&&"  ,
                      COLUMN 122,reg_tot[8]  USING  "#########&.&&&&&&"  ,
                      COLUMN 139,reg_tot[9]  USING  "#########&.&&&&&&"  ,
                      COLUMN 156,reg_tot[10] USING  "#########&.&&&&&&"  ,
                      COLUMN 173,reg_tot[11] USING  "#########&.&&&&&&"  ,
                      COLUMN 190,reg_tot[12] USING  "#########&.&&&&&&"  ,
                      COLUMN 207,reg_tot[13] USING  "#########&.&&&&&&" 

             PRINT COLUMN 02,"__________________________________________________", 
                             "__________________________________________________", 
                             "__________________________________________________", 
                             "__________________________________________________", 
                             "______________________"                         
                  

        ON LAST ROW 

              LET r_SB1                    =       0
              LET r_SB2                    =       0              
              LET r_SB3                    =       0
              LET r_SB4                    =       0
              LET r_SB5                    =       0
              LET r_SB6                    =       0
              LET r_SB7                    =       0             
              
              
           #----------------                       --------                      
           #-- Total de Movimientos Trasnsferidos a: ---#                        
                                                                                 
              LET r_tot_rec_sb1            =       0                             
              LET r_tot_rec_sb2            =       0                             
              LET r_tot_rec_sb3            =       0                             
              LET r_tot_rec_sb4            =       0                             
              LET r_tot_rec_sb5            =       0                             
              LET r_tot_rec_sb6            =       0                             
              LET r_tot_rec_sb7            =       0                             
              LET g_nivel                  =       0
              LET g_uni_nss                =       0
              
                                                                               
              
              DECLARE d5 CURSOR FOR                                              
                                                                                 
                 SELECT  a.nivel,COUNT(UNIQUE a.nss )                            
                 FROM    formatob a                                              
                 WHERE   a.tipo_traspaso IN ( 1,9 )                              
                 GROUP BY 1                                                      
                 ORDER BY 1                                                      
                                                                                 
              FOREACH d5 INTO g_nivel      ,                                        
                              g_uni_nss                                           
                                                                                 
                 CASE  g_nivel   
                                                                    
                    WHEN 1                                                 
                       LET  r_tot_rec_sb2     =  r_tot_rec_sb2 +  g_uni_nss
                    WHEN 2                                                 
                       LET  r_tot_rec_sb3     =  r_tot_rec_sb3 +  g_uni_nss
                    WHEN 3                                                 
                       LET  r_tot_rec_sb4     =  r_tot_rec_sb4 +  g_uni_nss                      
                    WHEN 4                                                 
                       LET  r_tot_rec_sb5     =  r_tot_rec_sb5 +  g_uni_nss
                    WHEN 5                                                       
                       LET  r_tot_rec_sb1     =  r_tot_rec_sb1 +  g_uni_nss   
                    WHEN 6                                                 
                       LET  r_tot_rec_sb3     =  r_tot_rec_sb3 +  g_uni_nss                             
                    WHEN 7                                                 
                       LET  r_tot_rec_sb4     =  r_tot_rec_sb4 +  g_uni_nss                      
                    WHEN 8                                                 
                       LET  r_tot_rec_sb5     =  r_tot_rec_sb5 +  g_uni_nss                       
                    WHEN 9                                                       
                       LET  r_tot_rec_sb1     =  r_tot_rec_sb1 +  g_uni_nss
                    WHEN 10                                   
                       LET  r_tot_rec_sb2     =  r_tot_rec_sb2 +  g_uni_nss         
                    WHEN 11                                                
                       LET  r_tot_rec_sb4     =  r_tot_rec_sb4 +  g_uni_nss                                             
                    WHEN 12                                                 
                       LET  r_tot_rec_sb5     =  r_tot_rec_sb5 +  g_uni_nss                                              
                    WHEN 13                                                      
                       LET  r_tot_rec_sb1     =  r_tot_rec_sb1 +  g_uni_nss      
                    WHEN 14  
                       LET  r_tot_rec_sb2     =  r_tot_rec_sb2 +  g_uni_nss  
                    WHEN 15                                                 
                       LET  r_tot_rec_sb3     =  r_tot_rec_sb3 +  g_uni_nss                                              
                    WHEN 16                                                 
                       LET  r_tot_rec_sb5     =  r_tot_rec_sb5 +  g_uni_nss                           
                    WHEN 17                                                      
                       LET  r_tot_rec_sb1     =  r_tot_rec_sb1 +  g_uni_nss      
                    WHEN 18 
                       LET  r_tot_rec_sb2     =  r_tot_rec_sb2 +  g_uni_nss                          
                    WHEN 19                                                 
                       LET  r_tot_rec_sb3     =  r_tot_rec_sb3 +  g_uni_nss                         
                    WHEN 20                                                
                       LET  r_tot_rec_sb4     =  r_tot_rec_sb4 +  g_uni_nss                        
                    WHEN 21                                                
                       LET  r_tot_rec_sb6     =  r_tot_rec_sb6 +  g_uni_nss                        
                    WHEN 22                                                
                       LET  r_tot_rec_sb7     =  r_tot_rec_sb7 +  g_uni_nss                                                                                      
                                           
                       
                 END CASE                                                        
                                                                                 
                                                                                 
                                                                                 
              END FOREACH                                                        
                                                                                 
                                                                                 
              LET r_tot_rec_s1            =       0                              
              LET r_tot_rec_s2            =       0                              
              LET r_tot_rec_s3            =       0                              
              LET r_tot_rec_s4            =       0                              
              LET r_tot_rec_s5            =       0                              
              LET r_tot_rec_s6            =       0                              
              LET r_tot_rec_s7            =       0  
              LET g_nivel                 =       0                            
              LET g_uni_nss               =       0                                                                   
              
              DECLARE d6 CURSOR FOR                                              
                                                                                 
                 SELECT  a.nivel,COUNT(UNIQUE a.nss )                            
                 FROM    formatob a                                              
                 WHERE   a.tipo_traspaso IN ( 0,2,3,4,5,6,7,8,10,11,12,13,14,15,16 )
                 GROUP BY 1                                                      
                 ORDER BY 1                                                      
                                                                                 
              FOREACH d6 INTO g_nivel   ,                                        
                              g_uni_nss                                          

               CASE  g_nivel
                
                  WHEN 1                                                  
                     LET  r_tot_rec_s2     =  r_tot_rec_s2 +  g_uni_nss 
                  WHEN 2                                                 
                     LET  r_tot_rec_s3     =  r_tot_rec_s3 +  g_uni_nss 
                  WHEN 3                                                
                     LET  r_tot_rec_s4     =  r_tot_rec_s4 +  g_uni_nss 
                  WHEN 4                                                
                     LET  r_tot_rec_s5     =  r_tot_rec_s5 +  g_uni_nss 
                  WHEN 5                                                
                     LET  r_tot_rec_s1     =  r_tot_rec_s1 +  g_uni_nss 
                  WHEN 6                                                
                     LET  r_tot_rec_s3     =  r_tot_rec_s3 +  g_uni_nss 
                  WHEN 7                                                
                     LET  r_tot_rec_s4     =  r_tot_rec_s4 +  g_uni_nss 
                  WHEN 8                                                
                     LET  r_tot_rec_s5     =  r_tot_rec_s5 +  g_uni_nss 
                  WHEN 9                                                
                     LET  r_tot_rec_s1     =  r_tot_rec_s1 +  g_uni_nss 
                  WHEN 10                                               
                     LET  r_tot_rec_s2     =  r_tot_rec_s2 +  g_uni_nss 
                  WHEN 11                                               
                     LET  r_tot_rec_s4     =  r_tot_rec_s4 +  g_uni_nss 
                  WHEN 12                                               
                     LET  r_tot_rec_s5     =  r_tot_rec_s5 +  g_uni_nss 
                  WHEN 13                                               
                     LET  r_tot_rec_s1     =  r_tot_rec_s1 +  g_uni_nss 
                  WHEN 14                                               
                     LET  r_tot_rec_s2     =  r_tot_rec_s2 +  g_uni_nss 
                  WHEN 15                                               
                     LET  r_tot_rec_s3     =  r_tot_rec_s3 +  g_uni_nss 
                  WHEN 16                                               
                     LET  r_tot_rec_s5     =  r_tot_rec_s5 +  g_uni_nss 
                  WHEN 17                                               
                     LET  r_tot_rec_s1     =  r_tot_rec_s1 +  g_uni_nss 
                  WHEN 18                                               
                     LET  r_tot_rec_s2     =  r_tot_rec_s2 +  g_uni_nss 
                  WHEN 19                                               
                     LET  r_tot_rec_s3     =  r_tot_rec_s3 +  g_uni_nss 
                  WHEN 20                                               
                     LET  r_tot_rec_s4     =  r_tot_rec_s4 +  g_uni_nss 
                  WHEN 21                                               
                     LET  r_tot_rec_s6     =  r_tot_rec_s6 +  g_uni_nss 
                  WHEN 22                                               
                     LET  r_tot_rec_s7     =  r_tot_rec_s7 +  g_uni_nss                                                                                                   
                 
                
                END CASE                      
                
                                                                                 
              END FOREACH                                                        
                                                                                 
                                                                                 
              LET r_SB1   = r_tot_rec_sb1           +      r_tot_rec_s1          
              LET r_SB2   = r_tot_rec_sb2           +      r_tot_rec_s2          
              LET r_SB3   = r_tot_rec_sb3           +      r_tot_rec_s3          
              LET r_SB4   = r_tot_rec_sb4           +      r_tot_rec_s4          
              LET r_SB5   = r_tot_rec_sb5           +      r_tot_rec_s5 
              LET r_SB6   = r_tot_rec_sb6           +      r_tot_rec_s6
              LET r_SB7   = r_tot_rec_sb7           +      r_tot_rec_s7
         



   

        SKIP 1 LINE

              PRINT COLUMN 02,"Total de movimientos Transferidas a:",
                    COLUMN 40,"   SB1    ",
                    COLUMN 55,"   SB2    ",
                    COLUMN 70,"   SB3    ",
                    COLUMN 85,"   SB4    ",
                    COLUMN 100,"   SB5    ",
                    COLUMN 115,"   SACP   ",
                    COLUMN 130,"   SALP   "

              PRINT COLUMN  40 ,r_SB1          USING "#####&" ,
                    COLUMN  55 ,r_SB2     USING "#####&" ,
                    COLUMN  70 ,r_SB3     USING "#####&" ,
                    COLUMN  85 ,r_SB4     USING "#####&" ,
                    COLUMN 100 ,r_SB5     USING "#####&" , 
                    COLUMN 115 ,r_SB6     USING "#####&" ,
                    COLUMN 130 ,r_SB7     USING "#####&"
           
             SKIP 2 LINES
{             
 
              LET    r_tot_solic      =         0

              SELECT COUNT(UNIQUE a.nss)
              INTO   r_tot_solic
                FROM tes_solicitud a,dis_cuenta b
              WHERE  a.fecha_traspaso BETWEEN fecha_ini AND fecha_fin
                AND  a.folio             =  b.folio
                AND  a.nss               =  b.nss
                AND  a.tipo_traspaso     IN ( 1,9 )
                AND  b.tipo_movimiento   =  1
                AND  a.estado            =  103
                AND  a.siefore_rec       IN ( 1,2,3,4,5,6 )

              PRINT COLUMN 02,"Total de Solicitudes:    ",r_tot_solic USING "#####&"
  
 
           PRINT COLUMN 02,"__________________________________________________",
                           "__________________________________________________",
                           "__________________________________________________",
                           "_______________________________________________"

           SKIP 2 LINES

}

           

           PRINT COLUMN 124,"Precios de Accion" 

           PRINT COLUMN  85,"   SB1    ",
                 COLUMN 100,"   SB2    ",
                 COLUMN 115,"   SB3    ",
                 COLUMN 130,"   SB4    ",
                 COLUMN 145,"   SB5    ",
                 COLUMN 160,"   SACP   ",
                 COLUMN 175,"   SALP   "

           PRINT COLUMN  85,pre_acc_1 USING "###.######",
                 COLUMN 100,pre_acc_2 USING "###.######",
                 COLUMN 115,pre_acc_3 USING "###.######",
                 COLUMN 130,pre_acc_4 USING "###.######",
                 COLUMN 145,pre_acc_5 USING "###.######",
                 COLUMN 160,pre_acc_6 USING "###.######",
                 COLUMN 175,pre_acc_7 USING "###.######" 
 
END REPORT

FUNCTION inicio()
#--i
DEFINE  fecha_env         DATE
DEFINE  fecha_retornada   DATE 

   LET  fecha_env          =        fecha_fin

   SELECT * 
   INTO reg_modulo.*
   FROM seg_modulo
   WHERE modulo_cod = 'est'

      CALL habil ( fecha_env ) #Trae ultimo dia habil del mes
      RETURNING  fecha_retornada

   LET  fecha_fin          =        fecha_retornada
  
   #----PRECIOS DE ACCION----#

   SELECT precio_del_dia
   INTO   pre_acc_1
   FROM   glo_valor_accion
   WHERE  fecha_valuacion     =  fecha_fin #ojo checar a q fecha se rpta
   AND    codigo_siefore      =  1

   IF (pre_acc_1 IS NULL) THEN
      LET pre_acc_1  =  0
   END IF

   SELECT precio_del_dia
   INTO   pre_acc_2
   FROM   glo_valor_accion
   WHERE  fecha_valuacion = fecha_fin #ojo checar a q fecha se rpta
   AND    codigo_siefore  = 2

   IF (pre_acc_2 IS NULL) THEN
      LET pre_acc_2  =  0
   END IF

   SELECT precio_del_dia
   INTO   pre_acc_3
   FROM   glo_valor_accion
   WHERE  fecha_valuacion = fecha_fin #ojo checar a q fecha se rpta
   AND    codigo_siefore  = 3

   IF (pre_acc_3 IS NULL) THEN
      LET pre_acc_3  =  0
   END IF

   SELECT precio_del_dia
   INTO   pre_acc_4  
   FROM   glo_valor_accion
   WHERE  fecha_valuacion = fecha_fin #ojo checar a q fecha se rpta
   AND    codigo_siefore    = 4  

   IF (pre_acc_4 IS NULL) THEN
      LET pre_acc_4   =  0
   END IF

   SELECT precio_del_dia
   INTO   pre_acc_5  
   FROM   glo_valor_accion
   WHERE  fecha_valuacion = fecha_fin #ojo checar a q fecha se rpta
   AND    codigo_siefore    = 5  

   IF (pre_acc_5 IS NULL) THEN
      LET pre_acc_5   =  0
   END IF

   SELECT precio_del_dia
   INTO   pre_acc_6  
   FROM   glo_valor_accion
   WHERE  fecha_valuacion = fecha_fin #ojo checar a q fecha se rpta
   AND    codigo_siefore    = 6  

   IF (pre_acc_6 IS NULL) THEN
      LET pre_acc_6   =  0
   END IF

   LET  pre_acc_7     =  0   #Siefore adicional 

   #----FIN PRECIOS DE ACCION----#

   WHENEVER ERROR CONTINUE
  
      DROP TABLE formatob


      CREATE  TEMP TABLE formatob  ( 
                                    nivel             SMALLINT        , 
                                    tip_col_mto       SMALLINT        ,
                                    tipo_traspaso     SMALLINT        ,
                                    monto_en_acciones DECIMAL(16,6)   ,
                                    nss               CHAR(11)
                                     )
   WHENEVER ERROR STOP
      

      #--Tipo Traspaso 1,9 ( Por ELECCION)
      #--Diferente a Tipo Traspaso <> 1,9 ( Por PROCESOS OPERATIVOS )

      LET     sql_texto =
            
      "INSERT INTO formatob ",

      "SELECT CASE  ",
               "WHEN a.siefore_ced = 1 AND a.siefore_rec  = 2 THEN 1 ",
               "WHEN a.siefore_ced = 1 AND a.siefore_rec  = 3 THEN 2 ",
               "WHEN a.siefore_ced = 1 AND a.siefore_rec  = 4 THEN 3 ",
               "WHEN a.siefore_ced = 1 AND a.siefore_rec  = 5 THEN 4 ",
               "WHEN a.siefore_ced = 2 AND a.siefore_rec  = 1 THEN 5 ",
               "WHEN a.siefore_ced = 2 AND a.siefore_rec  = 3 THEN 6 ",
               "WHEN a.siefore_ced = 2 AND a.siefore_rec  = 4 THEN 7 ",
               "WHEN a.siefore_ced = 2 AND a.siefore_rec  = 5 THEN 8 ",
               "WHEN a.siefore_ced = 3 AND a.siefore_rec  = 1 THEN 9 ",
               "WHEN a.siefore_ced = 3 AND a.siefore_rec  = 2 THEN 10 ",
               "WHEN a.siefore_ced = 3 AND a.siefore_rec  = 4 THEN 11 ",
               "WHEN a.siefore_ced = 3 AND a.siefore_rec  = 5 THEN 12 ",
               "WHEN a.siefore_ced = 4 AND a.siefore_rec  = 1 THEN 13 ",
               "WHEN a.siefore_ced = 4 AND a.siefore_rec  = 2 THEN 14 ",
               "WHEN a.siefore_ced = 4 AND a.siefore_rec  = 3 THEN 15 ",
               "WHEN a.siefore_ced = 4 AND a.siefore_rec  = 5 THEN 16 ",
               "WHEN a.siefore_ced = 5 AND a.siefore_rec  = 1 THEN 17 ",
               "WHEN a.siefore_ced = 5 AND a.siefore_rec  = 2 THEN 18 ",
               "WHEN a.siefore_ced = 5 AND a.siefore_rec  = 3 THEN 19 ",
               "WHEN a.siefore_ced = 5 AND a.siefore_rec  = 4 THEN 20 ",
               "WHEN a.siefore_ced = 1 AND a.siefore_rec  = 6 THEN 21 ",
             "END CASE, ",
             "CASE ",	
                "WHEN  b.subcuenta  IN ( 1,2,5,6,9 )   THEN 1 ",
                "WHEN  b.subcuenta  = 7                THEN 2 ",
                "WHEN  b.subcuenta  IN ( 30,31,32 )    THEN 3 ",
                "WHEN  b.subcuenta  IN ( 33,34 )       THEN 4 ",
                "WHEN  b.subcuenta  = 13               THEN 5 ",
                "WHEN  b.subcuenta  IN ( 3,10,22,23 )  THEN 6 ",
                "WHEN  b.subcuenta  IN ( 11,12,24,25 ) THEN 7 ",
                "WHEN  b.subcuenta  IN ( 15,16,26,27 ) THEN 8 ",
                "WHEN  b.subcuenta  IN ( 20,21,28,29 ) THEN 9 ",
             "END CASE, ",                             
             "a.tipo_traspaso, ",
             "b.monto_en_acciones,a.nss ",
      "FROM   tes_solicitud a,dis_cuenta b ",
      "WHERE  a.fecha_traspaso BETWEEN '",fecha_ini,"' AND '",fecha_fin, "' ",
        "AND  a.folio             =  b.folio ",
        "AND  a.nss               =  b.nss ",
        "AND  a.tipo_traspaso     IN ( 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16 ) ",
        "AND  b.tipo_movimiento   IN ( 1,55 ) ",
        "AND  a.estado            =  103 ",
        "AND  a.folio_solicitud   =  b.consecutivo_lote " CLIPPED

    
    PREPARE  etiqueta   FROM  sql_texto
    EXECUTE  etiqueta  
      
END FUNCTION

FUNCTION limpia_var()
#iv-----------
  
DEFINE  i   SMALLINT

FOR i = 1 TO 24

   LET reg_1[i].tot_x_elec          =                  0
   LET reg_1[i].tot_x_proc_oper     =                  0
   LET reg_1[i].tot_elecmasoper     =                  0
   -------  --------------- --------
   LET reg_1[i].rcv_imss            =                  0
   LET reg_1[i].seg_ret             =                  0
   LET reg_1[i].rcv_issste          =                  0
   LET reg_1[i].ahorro_solid        =                  0
   LET reg_1[i].ahorro_ret          =                  0
   LET reg_1[i].apor_vol            =                  0
   LET reg_1[i].compl_ret           =                  0
   LET reg_1[i].d_ahorro_larg_pla   =                  0
   LET reg_1[i].c_persp_larg_pla    =                  0
   LET reg_1[i].tot_sie_niv         =                  0

END FOR 

END FUNCTION 

FUNCTION habil(x_fecha)

    DEFINE x_fecha    DATE
    DEFINE sig_fecha    DATE
    DEFINE dia_semana SMALLINT
    DEFINE ant_habil  SMALLINT


    LET sig_fecha  = x_fecha

    WHILE TRUE

        LET dia_semana = WEEKDAY(sig_fecha)

        IF dia_semana = 0 OR dia_semana = 6 THEN
           LET sig_fecha  = sig_fecha - 1
           CONTINUE WHILE
        END IF

        SELECT "ok"
        FROM   tab_feriado
        WHERE  feria_fecha = sig_fecha

        IF STATUS <> NOTFOUND THEN
           LET sig_fecha  = sig_fecha - 1
           CONTINUE WHILE
        ELSE
           EXIT WHILE

        END IF
    END WHILE

    RETURN sig_fecha

END FUNCTION
#------------------- FECHA DE MODIFICACION :  07OCT 2010----------------------#
#LA MODIFICACION CONSISTIO EN AGREGAR EL TIPO_MOVIMIENTO = 55 CORRESPONDIENTE
#AL T.M  EXCLUSIVO DE LAS TES POR EDAD TIPO TRASPASO = 13.