#INCLUDE "GPER1030.CH"
#INCLUDE "RWMAKE.CH"
#INCLUDE "PROTHEUS.CH"
#INCLUDE "RPTDEF.CH"
#INCLUDE "FWPrintSetup.ch"
#IFNDEF CRLF
	#DEFINE CRLF ( chr(13)+chr(10) )
#ENDIF

#DEFINE Imp_Spool      	2
#DEFINE ALIGN_H_LEFT   	0
#DEFINE ALIGN_H_RIGHT  	1
#DEFINE ALIGN_H_CENTER 	2
#DEFINE ALIGN_V_CENTER 	0
#DEFINE ALIGN_V_TOP	   	1
#DEFINE ALIGN_V_BOTTON 	2

/*Rdmake correspondente ao fonte padrão GPER030.PRX; Alterações realizadas neste fonte devem ser compatibilizadas também no padrão.*/

/*/{Protheus.doc}Recibo
Emissão de Recibos de Pagamento
@type function
@author Zé Maria
@since 14/03/1995
@example Chamada padrão para programas em RdMake.
@history 22/03/2013, Ademar JR  , RHU210_09_13 - Unificacao dos fontes da fase padrao com a fase 4 (localizacoes).
@history 14/02/2017, M.Silveira , MRH-6871     - Geracao de LOG com as matriculas que nao foram enviadas por e-mail.
@history 10/03/2017, Oswaldo L  , MRH-7270     - Ajuste de nomes de campos e trativas países
@history 14/12/2017, Jaqueline L, DRHPAG-5066  - Unificação do Fonte para funcionar igual ao GPER030

/*/

User Function GPER030U(lTerminal,cFilTerminal,cMatTerminal,cProcTerminal, nRecTipo, cPerTerminal, cSemanaTerminal)

	// Define Variaveis Locais (Basicas)

	Local cString:="SRA"        // alias do arquivo principal (Base)
	Local aOrd   := {STR0001,STR0002,STR0003,STR0004,STR0005,STR0177,STR0178} //"Matricula"###"C.Custo"###"Nome"###"Chapa"###"C.Custo + Nome"###"Depto."###"Depto. + Nome"
	Local cDesc1 := STR0006		//"Emiss„o de Recibos de Pagamento."
	Local cDesc2 := STR0007		//"Ser  impresso de acordo com os parametros solicitados pelo"
	Local cDesc3 := STR0008		//"usu rio."
	Local aDriver:= ReadDriver(.T.)

	// Define Variaveis Locais (Programa)
	Local cIndCond
	Local cRotBlank := Space(GetSx3Cache( "RCH_ROTEIR", "X3_TAMANHO" ))
	Local Baseaux := "S", cDemit := "N"
	Local cHtml 	:= ""
	Local cDriver	:= SuperGetMv("MV_DRIVER",.F.,"EPSON")

	Private cMes	  := ""
	Private cAno    := ""
	Private aEmail  := {}

	// Define o numero da linha de impressão como 0
	SetPrc(0,0)

	// Define Variaveis Private(Basicas)
	Private aReturn  := {STR0009, 1,STR0010, 2, 2, 1, "",1 }	//"Zebrado"###"Administra‡„o"
	Private nomeprog :="GPER030"
	Private aLinha   := { }
	Private nLastKey := 0
	Private cPerg    :="GPER030"
	Private cSem_De  := "  /  /    "
	Private cSem_Ate := "  /  /    "
	Private nAteLim , nBaseFgts , nFgts , nBaseIr , nBaseIrFe , nTipRel

	//Raphael Ferreira - Oficina5 - Chamado - 105979
	Private nVbMgConsig

	Private cCompac := aDriver[1]
	Private cNormal := aDriver[2]
	
	

	Private cFilRCJ := ""

	// Define Variaveis Private(Programa)
	Private aLanca 			:= {}
	Private aProve 			:= {}
	Private aDesco 			:= {}
	Private aBases 			:= {}
	Private aInfo  			:= {}
	Private aCodFol			:= {}
	Private li     			:= _PROW()
	Private Titulo 			:= STR0011		//"EMISSŽO DE RECIBOS DE PAGAMENTOS"
	Private lEnvioOk 		:= .F.
	Private lRetCanc		:= .t.
	Private cIRefSem    	:= GetMv("MV_IREFSEM",,"S")
	Private aPerAberto		:= {}
	Private aPerFechado		:= {}
	Private cProcesso		:= "" // Armazena o processo selecionado na Pergunte GPR040 (mv_par01).
	Private cRoteiro		:= "" // Armazena o Roteiro selecionado na Pergunte GPR040 (mv_par02).
	Private cPeriodo		:= "" // Armazena o Periodo selecionado na Pergunte GPR040 (mv_par03).
	Private cCcto			:= ""
	Private cCond			:= ""
	Private cRot			:= ""
	Private lDepSf			:= Iif(SRA->(FieldPos("RA_DEPSF"))>0,.T.,.F.)
	Private lLpt1			:= .F.
	Private lFase4  		:= (cPaisLoc $ "ANG|ARG|COL|PTG|VEN")
	Private lUseCEI			:= .F.

	// Define Variaveis da Impressao Grafica
	Private oPrint
	Private _oPrint 		:= TMSPrinter():New( "Recibo Laser" )
	Private oArial08		:= TFont():New("Arial",08,08,,.F.,,,,.T.,.F.)//Normal
	Private oArial08N		:= TFont():New("Arial",08,08,,.T.,,,,.T.,.F.)//Negrito
	Private oArial10		:= TFont():New("Arial",10,10,,.F.,,,,.T.,.F.)//Normal
	Private oArial10N		:= TFont():New("Arial",10,10,,.T.,,,,.T.,.F.)//Negrito

	Private _oFont8   		:= TFont():New("Arial", 09, 08,   ,.F., ,   , ,    , .F.)
	Private _oFont8b  		:= TFont():New("Arial", 09, 08,.t.,.t., ,   , ,    , .F.)
	Private _oFont10 		:= TFont():New("Arial", 09, 10,.T.,.F., 5,.T., 5, .T., .F.)
	Private _oFont10b 		:= TFont():New("Arial", 09, 10,.T.,.t., 5,.T., 5, .T., .F.)
	Private _oFont12  		:= TFont():New("Arial", 09, 12,.T.,.F., 5,.T., 5, .T., .F.)
	Private _oFont14  		:= TFont():New("Arial", 09, 14,.T.,.T., 5,.T., 5, .T., .F.)
	Private _oFont16  		:= TFont():New("Arial", 09, 16,.T.,.F., 5,.T., 5, .T., .F.)
	Private _oBrush   		:= TBrush():New("",4)

	Private oPrinter		:= ""
	Private oFont1 			:=	TFont():New( "Verdana", 07, 07, , .F., , , , .T., .F. )//Verbas
	Private oFont2 			:=	TFont():New( "Verdana", 07, 07, , .F., , , , .T., .F. )//Cabeçalho e Rodapé
	Private oFont2N 		:= 	TFont():New( "Verdana", 07, 07, , .T., , , , .T., .F. )//Cabeçalho e Rodapé Negrito
	Private oFont3 			:=	TFont():New( "Verdana", 12, 12, , .T., , , , .T., .F. )//Titulo Interno
	Private oFont4 			:=	TFont():New( "Verdana", 14, 14, , .T., , , , .T., .F. )//Titulo Maior
	Private Semana
	Private cSemana
	Private lPDFEmail		:= .T. //PDF é por Email quando for .F. Visualizar o PDF no remote
	Private lIsDriver		:= Iif(cDriver == "EPSON",.T.,.F.)
	Private lContAnt        := .F.  //Controle para verificar se o recibo anterior é continuação

	_oPrint:SetPortrait() //SetLandscape() //

	IF SM0->M0_CODIGO == "01"
		Private _cFig        := "\system\unimed.bmp"
	ELSEIF SM0->M0_CODIGO == "02"
		Private _cFig        := "\system\lamel.bmp"
	ELSEIF SM0->M0_CODIGO == "03"
		Private _cFig        := "\system\panclinic.bmp"
	ELSEIF SM0->M0_CODIGO == "04"
		Private _cFig        := "\system\logo_sao_domingos.bmp"
	Endif

	Private _cont           := 3500
	Private _xCount          := 0

	// Envia controle para a funcao SETPRINT
	wnrel:="Recibo"            //Nome Default do relatorio em Disco

	// Verifica se o programa foi chamado do terminal - TCF
	lTerminal := If( lTerminal == Nil, .F., lTerminal )

	IF !( lTerminal )
		wnrel:=SetPrint(cString,wnrel,cPerg,Titulo,cDesc1,cDesc2,cDesc3,.F.,aOrd)
	Else
		If Select("SRC")>0
			SRC->(DbCloseArea())
		EndIf
	EndIF

	// Define a Ordem do Relatorio
	nOrdem := IF( !( lTerminal ), aReturn[8] , 1 )

	// Verifica as perguntas selecionadas
	If !IsBlind()
		Pergunte(cPerg, .F.)
	EndIf

	// Carregando variaveis mv_par?? para Variaveis do Sistema.
	cSemanaTerminal := IF( Empty( cSemanaTerminal ) , Space( Len( SRC->RC_SEMANA ) ) , cSemanaTerminal )
	cProcesso  := IF( !( lTerminal ), mv_par01 , cProcTerminal		)   //Processo
	cRoteiro   := IF( !( lTerminal ), mv_par02 , nRecTipo			)	//Emitir Recibos(Roteiro)
	cPeriodo   := IF( !( lTerminal ), mv_par03 , cPerTerminal		)   //Periodo
	Semana     := IF( !( lTerminal ), mv_par04 , cSemanaTerminal	)	//Numero da Semana
	cSemana    := Semana

	// Verificar se o sistema esta trabalhando com ou sem roteiro
	DbSelectArea( "RCH" )
	DbSetOrder( 4 )
	DbSeek(  xFilial( "RCH" ) + cProcesso + cRoteiro + cPeriodo + Semana, .F. )
	If Eof()
		DbSeek(  xFilial( "RCH" ) + cProcesso + cRotBlank + cPeriodo + Semana, .F. )
	Else
		cRotBlank := cRoteiro
	EndIf

	//Carregar os periodos abertos (aPerAberto) e/ou
	// os periodos fechados (aPerFechado), dependendo
	// do periodo (ou intervalo de periodos) selecionado
	RetPerAbertFech(cProcesso	,; // Processo selecionado na Pergunte.
	cRotBlank	,; // Roteiro selecionado na Pergunte.
	cPeriodo	,; // Periodo selecionado na Pergunte.
	Semana		,; // Numero de Pagamento selecionado na Pergunte.
	NIL			,; // Periodo Ate - Passar "NIL", pois neste relatorio eh escolhido apenas um periodo.
	NIL			,; // Numero de Pagamento Ate - Passar "NIL", pois neste relatorio eh escolhido apenas um numero de pagamento.
	@aPerAberto	,; // Retorna array com os Periodos e NrPagtos Abertos
	@aPerFechado ) // Retorna array com os Periodos e NrPagtos Fechados

	// Retorna o mes e o ano do periodo selecionado na pergunte.
	AnoMesPer(	cProcesso	,; // Processo selecionado na Pergunte.
	cRotBlank	,; // Roteiro selecionado na Pergunte.
	cPeriodo	,; // Periodo selecionado na Pergunte.
	@cMes		,; // Retorna o Mes do Processo + Roteiro + Periodo selecionado
	@cAno		,; // Retorna o Ano do Processo + Roteiro + Periodo selecionado
	Semana		 ) // Retorna a Semana do Processo + Roteiro + Periodo selecionado

	dDataRef   := CTOD("01/" + cMes + "/" + cAno)


	Esc        := 2 //cRoteiro //IF( !( lTerminal ), mv_par03 , nRecTipo		)	//Emitir Recibos(Adto/Folha/1¦/2¦/V.Extra)
	nTipRel    := IF( !( lTerminal ), mv_par05 , 4				)	//Tipo de Recibo (Pre-Impressão/Zebrado/EMail/PDF)
	cFilDe     := IF( !( lTerminal ), mv_par06,cFilTerminal		)	//Filial De
	cFilAte    := IF( !( lTerminal ), mv_par07,cFilTerminal		)	//Filial Ate
	cCcDe      := IF( !( lTerminal ), mv_par08,SRA->RA_CC		)	//Centro de Custo De
	cCcAte     := IF( !( lTerminal ), mv_par09,SRA->RA_CC		)	//Centro de Custo Ate
	cMatDe     := IF( !( lTerminal ), mv_par10,cMatTerminal		)	//Matricula Des
	cMatAte    := IF( !( lTerminal ), mv_par11,cMatTerminal		)	//Matricula Ate
	cNomDe     := IF( !( lTerminal ), mv_par12,SRA->RA_NOME		)	//Nome De
	cNomAte    := IF( !( lTerminal ), mv_par13,SRA->RA_NOME		)	//Nome Ate
	ChapaDe    := IF( !( lTerminal ), mv_par14,SRA->RA_CHAPA	)	//Chapa De
	ChapaAte   := IF( !( lTerminal ), mv_par15,SRA->RA_CHAPA	)	//Chapa Ate
	Mensag1    := mv_par16										 	//Mensagem 1
	Mensag2    := mv_par17											//Mensagem 2
	Mensag3    := mv_par18											//Mensagem 3
	cSituacao  := IF( !( lTerminal ),mv_par19, fSituacao( NIL , .F. ) )	//Situacoes a Imprimir
	cCategoria := IF( !( lTerminal ),mv_par20, fCategoria( NIL , .F. ))	//Categorias a Imprimir
	cBaseAux   := IF( !( lTerminal ),If(mv_par21 == 1,"S","N"),"S")	//Imprimir Bases
	cDeptoDe   := IF( !( lTerminal ),mv_par22,SRA->RA_DEPTO 	)	//Depto. De
	cDeptoAte  := IF( !( lTerminal ),mv_par23,SRA->RA_DEPTO 	)	//Depto. Ate
	lQuebraFun := IIF( !( lTerminal ) .And. !Empty(MV_PAR24),(MV_PAR24 == 1),.F.) //Quebra por Funcionário?

	nTipRel    := 4

	If aReturn[5] == 1 .and. nTipRel == 1
		li	:=  0
	EndIf

	If nTipRel == 4 //Apenas visualizar o PDF
		lPDFEmail	:= .F.
	EndIf

	limpArqPdf() //Limpar Recibos que por ventura ainda estejam no servidor

	cMesAnoRef := StrZero(Month(dDataRef),2) + StrZero(Year(dDataRef),4)
	If aReturn[5] == 3	//Impressao Direta na Porta
		lLpt1 := .T.
	EndIf

	IF !( lTerminal )
		// Inicializa Impressao
		If ! fInicia(cString,nTipRel)
			Return
		Endif
	EndIF

	DbSelectArea( "SRA" )

	RptStatus({|lEnd| R030Imp(@lEnd,wnRel,cString,cMesAnoRef,.f.)},Titulo)  // Chamada do Relatorio

Return( IF( lTerminal , cHtml , NIL ) )

/*/{Protheus.doc} R030Imp
Processamento Para emissao do Recibo
@type Function
@param lEnd
@param WnRel
@param cString
@param cMesAnoRef
@param lTerminal
@author Zé Maria
@since 14/03/1995
@version Generic
/*/

Static Function R030Imp(lEnd,WnRel,cString,cMesAnoRef,lTerminal)

	// Define Variaveis Locais (Basicas)
	Local aCodBenef   	:= {}
	Local aTInss	  	:= {}
	Local cAcessaSR1  	:= &("{ || " + ChkRH("GPER030","SR1","2") + "}")
	Local cAcessaSRA  	:= &("{ || " + ChkRH("GPER030","SRA","2") + "}")
	Local cAcessaSRC  	:= &("{ || " + ChkRH("GPER030","SRC","2") + "}")
	Local cAcessaSRD  	:= &("{ || " + ChkRH("GPER030","SRD","2") + "}")
	Local cNroHoras   	:= &("{ || If(aVerbasFunc[nReg,5] > 0 .And. cIRefSem == 'S', aVerbasFunc[nReg,5], aVerbasFunc[nReg,6]) }")
	Local cHtml		  	:= ""
	Local nHoras      	:= 0
	Local nMes, nAno
	Local nX
	Local nReg		  	:= 0
	Local cPerAnt	  	:= ""
	Local cKey		  	:= ""
	Local cInicio	  	:= ""
	Local nBInssPA	  	:= 0 //Teto da base de INSS dos pro-labores/autonomos
	Local dDataLibRh
	Local cPerCorrente	:= ""
	Local cSemCorrente	:= ""
	Local cMesCorrente 	:= ""
	Local cAnoCorrente 	:= ""

	Local nTcfDadt		:= if(lTerminal,getmv("MV_TCFDADT",,0),0)		// indica o dia a partir do qual esta liberada a consulta ao TCF
	Local nTcfDfol		:= if(lTerminal,getmv("MV_TCFDFOL",,0),0)		// indica a quantidade de dias a somar ou diminuir no ultimo dia do mes corrente para liberar a consulta do TCF
	Local nTcfD131		:= if(lTerminal,getmv("MV_TCFD131",,0),0)		// indica o dia a partir do qual esta liberada a consulta ao TCF
	Local nTcfD132		:= if(lTerminal,getmv("MV_TCFD132",,0),0)		// indica o dia a partir do qual esta liberada a consulta ao TCF
	Local nTcfDext		:= if(lTerminal,getmv("MV_TCFDEXT",,0),0)		// indica o dia a partir do qual esta liberada a consulta ao TCF
	Local nDec
	Local nPosMsg1		:= 0
	Local nPosMsg2		:= 0
	Local nPosMsg3		:= 0
	Local cFilter	   	:= aReturn[7]
	Local cPath			:= supergetmv("MV_RELT",,"\spool\")
	//Raphael Ferreira - Oficina5 - Chamado - 105979
	Local cVbMgConsig	:= AllTrim(supergetmv("MV_Y_VBCS",,"M01"))

	Private tamanho   	:= "M"
	Private limite		:= 132
	Private cDtPago   	:= ""
	Private cTipoRot 	:= PosAlias("SRY", cRoteiro, SRA->RA_FILIAL, "RY_TIPO")
	Private aVerbasFunc := {}
	Private aTitle		:= {}
	Private aLog		:= {}
	Private cRotBlank
	Private nHraExtra 	:= 0
	Private nPagoDom	:= 0

	Private cPict1		:= "@E 999,999,999.99"
	Private cPict2 		:= "@E 9,999,999.99"
	Private cPict3 		:= "@E 999,999.99"

	Private nSalario		:= 0
	Private cPathTmp		:=	""

	//-Ordem 1 -> RCA_FILIAL+RCA_MNEMON
	If FPOSREG("RCA", 1, XFILIAL("RCA")+"RHDECIMAIS")
		nDec := Val(Alltrim(RCA->RCA_CONTEU))
	Else
		nDec := MsDecimais(1)
	EndIf

	If nDec = 0
		cPict1	:=	"@E 99,999,999,999"
		cPict2 	:=	"@E 9,999,999,999"
		cPict3 	:=	"@E 99,999,999"
	Endif

	// Ajuste do tipo da variavel
	nTcfDadt	:= if(valtype(ntcfdadt)=="C",val(ntcfdadt),ntcfdadt)
	nTcfD131	:= if(valtype(nTcfD131)=="C",val(nTcfD131),nTcfD131)
	nTcfD132	:= if(valtype(nTcfD132)=="C",val(nTcfD132),nTcfD132)
	nTcfDfol	:= if(valtype(ntcfdfol)=="C",val(ntcfdfol),ntcfdfol)
	nTcfDext	:= if(valtype(ntcfdext)=="C",val(ntcfdext),ntcfdext)

	// Verifica se o Mes solicitado esta liberado para consulta no terminal de consulta do funcionario.

	If lTerminal

		dbSelectArea("RCH")
		dbSetOrder(6)
		IF cPaisLoc == "ARG"
			cKey := "xFilial('RCH') + cProcesso"
			cInicio := "RCH_FILIAL + RCH_PROCES"
		Else
			cKey    := "xFilial('RCH') + cProcesso" + IIf(cRoteiro == "EXT", "", " + cRoteiro")
			cInicio := "RCH_FILIAL + RCH_PROCES" + IIf(cRoteiro == "EXT", "", " + RCH_ROTEIR")
		Endif

		DbSeek( &(cKey))
		If Eof()
			cRotBlank 	:= Space(GetSx3Cache( "RCH_ROTEIR", "X3_TAMANHO" ))
			cKey := "xFilial('RCH') + cProcesso " + IIf(cRoteiro == "EXT", "", " + cRotBlank")
			cInicio := "RCH_FILIAL + RCH_PROCES " + IIf(cRoteiro == "EXT", "", " + RCH_ROTEIR")
			DbSeek( &(cKey))
		EndIf
		If !Eof()
			While RCH->( !Eof() .and. &(cInicio) == &(cKey))
				If Empty(RCH->RCH_DTFECH)
					cPerCorrente := RCH->RCH_PER
					cSemCorrente := RCH->RCH_NUMPAG
					cMesCorrente := RCH->RCH_MES
					cAnoCorrente := RCH->RCH_ANO
					Exit
				EndIf
				RCH->( dbSkip() )
			EndDo
		EndIf

		If	(cPerCorrente == cPeriodo .AND. cSemCorrente == Semana)  .OR. ;
				( MesAno(dDataRef) >= (cAnoCorrente + cMesCorrente) )

			If  cTipoRot == "2" //Adiantamento
				If ( MesAno(dDataRef) > (cAnoCorrente + cMesCorrente) ) .Or.;
						( If(MesAno(Date()) == (cAnoCorrente + cMesCorrente), Day(Date()) < nTCFDADT,.F.) )
					Return( IF( lTerminal <> NIL .And. lTerminal , cHtml , NIL ) )
				EndIf
			ElseIf ( cTipoRot == "1" .Or. cTipoRot == "9" ) .and. !empty(nTCFDFOL) //Folha
				dDataLibRh := fMontaDtTcf(cMesCorrente + cAnoCorrente,nTCFDFOL)
				If Date() < dDataLibRH
					Return( IF( lTerminal <> NIL .And. lTerminal , cHtml , NIL ) )
				Endif
			ElseIf cTipoRot == "5" //1a parcela 13o Salario
				If ( MesAno(dDataRef) > (cAnoCorrente + cMesCorrente) ) .Or.;
						( If(MesAno(Date()) == (cAnoCorrente + cMesCorrente), Day(Date()) < nTCFD131,.F.) )
					Return( IF( lTerminal <> NIL .And. lTerminal , cHtml , NIL ) )
				Endif
			ElseIf cTipoRot == "6" //2a parcela 13o Salario
				If ( MesAno(dDataRef) > (cAnoCorrente + cMesCorrente) ) .Or.;
						( If(MesAno(Date()) == (cAnoCorrente + cMesCorrente), Day(Date()) < nTCFD132,.F.) )
					Return( IF( lTerminal <> NIL .And. lTerminal , cHtml , NIL ) )
				Endif
			ElseIf cRoteiro == "EXT"  // Valores Extras
				If ( MesAno(dDataRef) > (cAnoCorrente + cMesCorrente) ) .Or.;
						( If(MesAno(Date()) == (cAnoCorrente + cMesCorrente), Day(Date()) < nTCFDEXT,.F.) )
					Return( IF( lTerminal <> NIL .And. lTerminal , cHtml , NIL ) )
				Endif
			EndIf
		Endif
	Endif

	// Selecionando a Ordem de impressao escolhida no parametro.
	dbSelectArea( "SRA")
	IF !( lTerminal )
		If nOrdem == 1			//"Matricula"
			dbSetOrder(1)
		ElseIf nOrdem == 2		//"C.Custo"
			dbSetOrder(2)
		ElseIf nOrdem == 3		//"Nome"
			dbSetOrder(3)
		Elseif nOrdem == 4		//"Chapa"
			cArqNtx  := CriaTrab(NIL,.f.)
			cIndCond :="RA_Filial + RA_Chapa + RA_Mat"
			IndRegua("SRA",cArqNtx,cIndCond,,,STR0012)		//"Selecionando Registros..."

		ElseIf nOrdem == 5		//"C.Custo + Nome"
			dbSetOrder(8)
		ElseIf nOrdem == 6		//"Depto"
			dbSetOrder(21)
		ElseIf nOrdem == 7		//"Depto. + Nome"
			dbSetOrder(22)
		Endif

		dbGoTop()

	EndIF

	// Selecionando o Primeiro Registro e montando Filtro.
	If nOrdem == 1 .or. lTerminal
		cInicio := "SRA->RA_FILIAL + SRA->RA_MAT"
		IF !( lTerminal )
			dbSeek(cFilDe + cMatDe,.T.)
			cFim    := cFilAte + cMatAte
		Else
			cFim    := &(cInicio)
		EndIF
	ElseIf nOrdem == 2
		dbSeek(cFilDe + cCcDe + cMatDe,.T.)
		cInicio  := "SRA->RA_FILIAL + SRA->RA_CC + SRA->RA_MAT"
		cFim     := cFilAte + cCcAte + cMatAte
	ElseIf nOrdem == 3
		dbSeek(cFilDe + cNomDe + cMatDe,.T.)
		cInicio := "SRA->RA_FILIAL + SRA->RA_NOME + SRA->RA_MAT"
		cFim    := cFilAte + cNomAte + cMatAte
	ElseIf nOrdem == 4
		dbSeek(cFilDe + ChapaDe + cMatDe,.T.)
		cInicio := "SRA->RA_FILIAL + SRA->RA_CHAPA + SRA->RA_MAT"
		cFim    := cFilAte + ChapaAte + cMatAte
	ElseIf nOrdem == 5
		dbSeek(cFilDe + cCcDe + cNomDe,.T.)
		cInicio  := "SRA->RA_FILIAL + SRA->RA_CC + SRA->RA_NOME"
		cFim     := cFilAte + cCcAte + cNomAte
	ElseIf nOrdem == 6
		dbSeek(cFilDe + cDeptoDe + cMatDe,.T.)
		cInicio  := "SRA->RA_FILIAL + SRA->RA_DEPTO + SRA->RA_MAT"
		cFim     := cFilAte + cDeptoAte + cMatAte
	ElseIf nOrdem == 7
		dbSeek(cFilDe + cDeptoDe + cNomDe,.T.)
		cInicio  := "SRA->RA_FILIAL + SRA->RA_DEPTO + SRA->RA_NOME"
		cFim     := cFilAte + cDeptoAte + cNomAte
	Endif

	dbSelectArea("SRA")

	// Carrega Regua Processamento

	#IFDEF TOP
		cAliasTMP := "QNRO"
		BeginSql alias cAliasTMP
			SELECT COUNT(*) as NROREG
			FROM %table:SRA% SRA
			WHERE      SRA.RA_FILIAL BETWEEN %exp:cFilDe%   AND %exp:cFilAte%
			AND SRA.RA_MAT    BETWEEN %exp:cMatDe%   AND %exp:cMatAte%
			AND SRA.RA_CC     BETWEEN %exp:cCCDe%    AND %exp:cCCAte%
			AND SRA.RA_DEPTO  BETWEEN %exp:cDeptoDe% AND %exp:cDeptoAte%
			AND SRA.%notDel%
		EndSql

		nRegProc := (cAliasTMP)->(NROREG)
		( cAliasTMP )->( dbCloseArea() )
		IF nTipRel # 3
			SetRegua(nRegProc)	// Total de elementos da regua
		Else
			IF !( lTerminal )
				GPProcRegua(nRegProc)// Total de elementos da regua
			EndIF
		EndIF

		dbSelectArea("SRA")

	#ELSE
		SetRegua(RecCount())	// Total de elementos da regua

		dbSelectArea("SRA")

	#ENDIF

	TOTVENC:= TOTDESC:= FLAG:= CHAVE := 0

	Desc_Fil := Desc_End := DESC_CC:= DESC_FUNC:= ""
	Desc_Comp:= Desc_Est := Desc_Cid:= ""
	DESC_MSG1:= DESC_MSG2:= DESC_MSG3:= Space(01)
	cFilialAnt := Space(FwGetTamFilial)
	Vez        := 0
	OrdemZ     := 0

	While SRA->( !Eof() .And. &cInicio <= cFim )

		// Movimenta Regua Processamento
		IF !( lTerminal )

			IncRegua()  // Anda a regua

			If lEnd
				@Prow()+1,0 PSAY cCancel
				Exit
			Endif

			// Consiste Filtro da setprint
			If ! Empty(cFilter) .And. ! SRA->(&(cFilter))
				dbSelectArea("SRA")
				dbSkip()
				Loop
			EndIf

			// Consiste Parametrizacao do Intervalo de Impressao
			If (SRA->RA_CHAPA < ChapaDe)   .Or. (SRA->RA_CHAPa > ChapaAte) .Or. ;
					(FtAcento( SRA->RA_NOME )   < cNomDe) .Or. (FtAcento( SRA->RA_NOME ) > cNomAte)   .Or. ;
					(SRA->RA_MAT < cMatDe)     .Or. (SRA->RA_MAT > cMatAte)    .Or. ;
					(SRA->RA_CC < cCcDe)       .Or. (SRA->RA_CC > cCcAte)      .Or. ;
					(SRA->RA_DEPTO < cDeptoDe) .Or. (SRA->RA_DEPTO > cDeptoAte)

				SRA->(dbSkip(1))
				Loop
			EndIf

		EndIF

		aLanca:={}         // Zera Lancamentos
		aProve:={}         // Zera Lancamentos
		aDesco:={}         // Zera Lancamentos
		aBases:={}         // Zera Lancamentos
		nAteLim := nBaseFgts := nFgts := nBaseIr := nBaseIrFe := 0.00

		//Raphael Ferreira - Oficina5 - Chamado - 105979
		nVbMgConsig := 0.00

		Ordem_rel := 1     // Ordem dos Recibos

		// Verifica Data Demissao
		cSitFunc := SRA->RA_SITFOLH
		dDtPesqAf:= CTOD("01/" + Left(cMesAnoRef,2) + "/" + Right(cMesAnoRef,4),"DDMMYY")
		If cSitFunc == "D" .And. (!Empty(SRA->RA_DEMISSA) .And. MesAno(SRA->RA_DEMISSA) > MesAno(dDtPesqAf))
			cSitFunc := " "
		Endif

		//-Busca o Salario Base do Funcionario
		nSalario := fBuscaSal(dDataRef,,,.F.)
		If nSalario == 0
			nSalario := SRA->RA_SALARIO
		EndIf

		IF !( lTerminal )

			// Consiste situacao e categoria dos funcionarios
			If !( cSitFunc $ cSituacao ) .OR.  ! ( SRA->RA_CATFUNC $ cCategoria )
				SRA->(dbSkip())
				Loop
			Endif
			If cSitFunc $ "D" .And. Mesano(SRA->RA_DEMISSA) # Mesano(dDataRef)
				SRA->(dbSkip())
				Loop
			Endif

			// Consiste controle de acessos e filiais validas
			If !(SRA->RA_FILIAL $ fValidFil()) .Or. !Eval(cAcessaSRA)
				SRA->(dbSkip())
				Loop
			EndIf

		EndIF

		If SRA->RA_Filial # cFilialAnt
			If ! Fp_CodFol(@aCodFol,Sra->Ra_Filial) .Or. ! fInfo(@aInfo,Sra->Ra_Filial)
				Return Nil
			Endif
			Desc_Fil := Alltrim(If( lLpt1, fTAcento(aInfo[3]), aInfo[3] ))
			Desc_End := Alltrim(If( lLpt1, fTAcento(aInfo[4]), aInfo[4] ))	// Dados da Filial
			lUseCEI  := If( Len(aInfo) >= 27 .And. !Empty( aInfo[27] ), ( If(Len(aInfo) >= 28, If(aInfo[28] == 1, .T., .F.) , .T.) ), .F. )
			//Desc_CGC := If( lUseCEI , aInfo[27], aInfo[8] )
			Desc_CGC := " CNPJ: " + Transform(If( lUseCEI , aInfo[27], aInfo[8] ), "@R 99.999.999/9999-99")
			Desc_IE  :=  "I.E.: " + Transform(ainfo[9], "@R 999.999.999.999") // IE
			DESC_MSG1:= DESC_MSG2:= DESC_MSG3:= Space(01)
			Desc_Est := Alltrim(Substr(fDesc("SX5","12"+If( lLpt1, fTAcento(aInfo[6]), aInfo[6] ),"X5DESCRI()"),1,20))
			Desc_Comp:= Alltrim(If( lLpt1, fTAcento(aInfo[14]), aInfo[14] ))	// Complemento Cobranca
			Desc_Cid := Alltrim(If( lLpt1, fTAcento(aInfo[5]), aInfo[5] ))
		End_Compl:= Alltrim(aInfo[4] + " " + aInfo[13] + " " + aInfo[05] + " " + aInfo[06] + " " + aInfo[07])  //endereço + bairro + cidade + estado + cep
		Desc_EndC:= Alltrim(If( lLpt1, fTAcento( End_Compl ), End_Compl ))

		Desc_CEP := "CEP: " + Transform(ainfo[7], "@R 99999-999") // CEP
		Desc_Bairro := Alltrim(aInfo[13])

		// MENSAGENS
		If !Empty(MENSAG1)

			nPosMsg1		:= fPosTab("S036",Alltrim(MENSAG1), "==", 4)
			If nPosMsg1 > 0
				DESC_MSG1	:= fTabela("S036",nPosMsg1,5,dDataRef)
			EndIf
		Endif

		nPosMsg2		:= fPosTab("S036",Alltrim(MENSAG2), "==", 4)
		If nPosMsg2 > 0
			DESC_MSG2	:= fTabela("S036",nPosMsg2,5,dDataRef)
		EndIf

		nPosMsg3		:= fPosTab("S036",Alltrim(MENSAG3), "==", 4)
		If nPosMsg3 > 0
			DESC_MSG3	:= fTabela("S036",nPosMsg3,5,dDataRef)
		EndIf

		dbSelectArea("SRA")
		cFilialAnt := SRA->RA_FILIAL
	Endif

	Totvenc := Totdesc := 0

	If Len(aTinss) > 0
		nBInssPA := aTinss[Len(aTinss),1]
	EndIf

	//Retorna as verbas do funcionario, de acordo com os periodos selecionados
	aVerbasFunc	:= RetornaVerbasFunc(	SRA->RA_FILIAL					,; // Filial do funcionario corrente
	SRA->RA_MAT	  					,; // Matricula do funcionario corrente
	NIL								,; //
	cRoteiro	  					,; // Roteiro selecionado na pergunte
	NIL			  					,; // Array com as verbas que deverão ser listadas. Se NIL retorna todas as verbas.
	aPerAberto	  					,; // Array com os Periodos e Numero de pagamento abertos
	aPerFechado	 	 				,; // Array com os Periodos e Numero de pagamento fechados
	NIL								,;
		NIL								,;
		.T.)								// Controle para verificar se conteúdo veio transferido de outra empresa

	If cRoteiro <> "EXT"
		For nReg := 1 to Len(aVerbasFunc)
			If (Len(aPerAberto) > 0 .AND. !Eval(cAcessaSRC)) .OR. (Len(aPerFechado) > 0 .AND. !Eval(cAcessaSRD)) .Or.;
					( aVerbasFunc[nReg,7] <= 0 )
				dbSkip()
				Loop
			EndIf

			If PosSrv( aVerbasFunc[nReg,3] , SRA->RA_FILIAL , "RV_TIPOCOD" ) == "1"
				nHoras := Eval(cNroHoras)
				fSomaPdRec("P",aVerbasFunc[nReg,3],nHoras,aVerbasFunc[nReg,7])
				TOTVENC += aVerbasFunc[nReg,7]

			Elseif SRV->RV_TIPOCOD == "2"
				fSomaPdRec("D",aVerbasFunc[nReg,3],aVerbasFunc[nReg,6],aVerbasFunc[nReg,7])
				TOTDESC += aVerbasFunc[nReg,7]

			Elseif SRV->RV_TIPOCOD $ "3/4"
				//No Paraguai imprimir somente o valor liquido
				If cPaisLoc <> "PAR" .Or. (aVerbasFunc[nReg,3] == aCodFol[047,1])
					fSomaPdRec("B",aVerbasFunc[nReg,3],aVerbasFunc[nReg,6],aVerbasFunc[nReg,7])
				Endif
			Endif

			If (aVerbasFunc[nReg,3] $ aCodFol[10,1]+'*'+aCodFol[15,1]+'*'+aCodFol[27,1])
				nBaseIr += aVerbasFunc[nReg,7]
			ElseIf (aVerbasFunc[nReg,3] $ aCodFol[13,1]+'*'+aCodFol[19,1]+'*'+aCodFol[221,1]+'*'+aCodFol[225,1])
				nAteLim += aVerbasFunc[nReg,7]
			Elseif (aVerbasFunc[nReg,3] $ aCodFol[108,1]+'*'+aCodFol[17,1])
				nBaseFgts += aVerbasFunc[nReg,7]
			Elseif (aVerbasFunc[nReg,3] $ aCodFol[109,1]+'*'+aCodFol[18,1])
				nFgts += aVerbasFunc[nReg,7]
			Elseif (aVerbasFunc[nReg,3] == aCodFol[16,1])
				nBaseIrFe += aVerbasFunc[nReg,7]
			ElseIf (aVerbasFunc[nReg,3]	== cVbMgConsig)//Raphael Ferreira - Oficina5 - Chamado - 105979
				nVbMgConsig += aVerbasFunc[nReg,7]
			Endif
		Next nReg

	Elseif cRoteiro == "EXT"
		dbSelectArea("SR1")
		dbSetOrder(1)
		If dbSeek( SRA->RA_FILIAL + SRA->RA_MAT )
			While !Eof() .And. SRA->RA_FILIAL + SRA->RA_MAT ==	SR1->R1_FILIAL + SR1->R1_MAT
				If Semana # "99"
					If SR1->R1_SEMANA # Semana
						dbSkip()
						Loop
					Endif
				Endif
				If !Eval(cAcessaSR1)
					dbSkip()
					Loop
				EndIf
				If PosSrv( SR1->R1_PD , SRA->RA_FILIAL , "RV_TIPOCOD" ) == "1"
					fSomaPdRec("P",SR1->R1_PD,SR1->R1_HORAS,SR1->R1_VALOR)
					TOTVENC = TOTVENC + SR1->R1_VALOR
				Elseif SRV->RV_TIPOCOD == "2"
					fSomaPdRec("D",SR1->R1_PD,SR1->R1_HORAS,SR1->R1_VALOR)
					TOTDESC = TOTDESC + SR1->R1_VALOR
				Elseif SRV->RV_TIPOCOD $ "3/4"
					fSomaPdRec("B",SR1->R1_PD,SR1->R1_HORAS,SR1->R1_VALOR)
				Endif
				dbskip()
			Enddo
		Endif
	Endif

	dbSelectArea("SRA")

	If TOTVENC = 0 .And. TOTDESC = 0
		dbSkip()
		Loop
	Endif

	If Vez == 0 .And. (cTipoRot == "1" .Or. cTipoRot == "9")	//--> Verifica se for FOLHA.
		PerSemana() // Carrega Datas referentes a Semana.
	EndIf

	If (nTipRel == 4 .OR. nTipRel == 5) .and. !( lTerminal ) //4 Visualizar o PDF 5 Enviar PDF por email
		fImprPDF(lPDFEmail,cPath)
		ASize(AProve,0)
		ASize(ADesco,0)
		ASize(aBases,0)
	Endif

	dbSelectArea("SRA")
	SRA->( dbSkip() )
	if(lQuebraFun) .AND. aReturn[5] <> 3  /*Se desejar imprimir um comprovante por folha...ignorada quando impresso direto na porta (formulario continuo)*/
		
		_cont :=3500     //FLUIG - 383098 INCLUIR CPF - GUILHERME e ADICIONAR QUEBRA DE PAGINA
		_oPrint:EndPage()
		
		__Eject()
		LI := 4
		SetPrc(LI,0)
	endIf

	TOTDESC := TOTVENC := 0
EndDo

If nTipRel == 4 .and. !( lTerminal )
	IF Type("_oPrint") == "O"
		_oPrint:Preview()
		_oPrint:EndPage()
		FreeObj(_oPrint)
		_oPrint := Nil
	Else
		MSGALERT( STR0203, STR0202 ) //"Não foram localizados demonstrativos de pagamentos disponíveis para esta consulta."  -  "Atenção!"
	EndIf
EndIf

IF !( lTerminal ).And.cPaisLoc<>"PTG"

	// Termino do relatorio
	dbSelectArea("SRC")
	dbSetOrder(1)          // Retorno a ordem 1
	dbSelectArea("SRD")
	dbSetOrder(1)          // Retorno a ordem 1
	dbSelectArea("SRA")
	SET FILTER TO
	RetIndex("SRA")

	If !(Type("cArqNtx") == "U")
		fErase(cArqNtx + OrdBagExt())
	Endif

	Set Device To Screen

	If lEnvioOK
		APMSGINFO(STR0042)
	ElseIf nTipRel== 3
		APMSGINFO(STR0043)
	EndIf

	//Gera LOG dos funcionarios que nao foram enviados
	If !Empty(aLog) .And. MsgYesNo(OemToAnsi("Deseja imprimir o LOG das matrículas que não foram enviadas?"),OemToAnsi("Atenção")) //"Deseja imprimir o LOG das matrículas que não foram enviadas?"#"Atenção"
		fMakeLog(aLog,aTitle,,,"GPER030LOG",OemToAnsi("LOG das matrículas que não foram enviadas"),"G","L",,.F.) //"LOG das matrículas que não foram enviadas"
	EndIf

	SeTPgEject(.F.)
	nlin:= 0
	If aReturn[5] = 1 .and. (nTipRel == 1 .OR.  nTipRel == 2)
		Set Printer To
		Commit
		ourspool(wnrel)
	Endif
	MS_FLUSH()

EndIF

Return( cHtml )


/*/{Protheus.doc} PerSemana
//TODO Descrição auto-gerada.
@type function
/*/
Static Function PerSemana() // Pesquisa datas referentes a semana.
	Local aAreaRCF	:= RCF->(GetArea())
	Local cChaveSem	:= ""

	dbSelectArea( "RCF" )
	dbSetOrder(2)//RCF_FILIAL, RCF_PROCES, RCF_PER, RCF_ROTEIR, RCF_TNOTRA, RCF_SEMANA

	If !Empty(Semana)
		// Turno do funcionario
		cChaveSem := cProcesso + StrZero(Year(dDataRef),4)+StrZero(Month(dDataRef),2)+Space(Tamsx3("RCF_ROTEIR")[1])+SRA->RA_TNOTRAB

		If !dbSeek(xFilial("RCF") + cChaveSem + Semana, .T. )
			cChaveSem := cProcesso + StrZero(Year(dDataRef),4)+StrZero(Month(dDataRef),2)+Space(Tamsx3("RCF_ROTEIR")[1])+"@@@"
			If !dbSeek(xFilial("RCF") + cChaveSem + Semana  )
				HELP( " ",1,"GPCALEND",  )	// "Nao existe calendario cadastrado para a competencia,  Turno ou semana."
				Return(NIL)
			Endif
		Endif
		cSem_De  := DtoC(RCF->RCF_DTINI,'DDMMYY')
		cSem_Ate := DtoC(RCF->RCF_DTFIM,'DDMMYY')
	EndIf

	RestArea(aAreaRCF)

Return Nil

/*/{Protheus.doc} fSomaPdRec
Somar as Verbas no Array
@type Function
@author Mauro
@param Tipo
@param cPd
@param nHoras
@param nValor
@since 24/09/1995
@version Generic
/*/

Static Function fSomaPdRec(cTipo,cPd,nHoras,nValor)

	Local Desc_paga
	Local cVbEmp    := SUPERGETMV("O5_VBEMPCO",,"547/548")

	Static lAglutPd // Sera utilizada em todas as chamadas da funcao fSomaPdRec()

	If lAglutPd == Nil
		lAglutPd := ( GetMv("MV_AGLUTPD",,"1") == "1" ) // 1-Aglutina verbas   2-Nao Aglutina
	EndIf

	Desc_paga := DescPd(cPd,Sra->Ra_Filial)  // mostra como pagto
/*Compilar depois parte para trazer parcela do emprestimo comsignado
	If cPd $ cVbEmp
		DBSelectArea("SRC")
		DbSetOrder(1)
		If DbSeek(xFilial("SRC") + SRA->RA_MAT + cPd)
			If cPeriodo == SRC->RC_PERIODO
				Desc_paga := ALLTRIM(Desc_Paga) + " - PARCELA " + SRC->RC_Y_EMCON
			Else
				DBSelectArea("SRD")
				DbSetOrder(1)
				If DbSeek(xFilial("SRD") + SRA->RA_MAT + cPeriodo + cPd)
					Desc_paga := ALLTRIM(Desc_Paga) + " - PARCELA " + SRD->RD_Y_EMCON
				EndIf
			EndIf
		Else
			DBSelectArea("SRD")
			DbSetOrder(1)
			If DbSeek(xFilial("SRD") + SRA->RA_MAT + cPeriodo + cPd)
				Desc_paga := ALLTRIM(Desc_Paga) + " - PARCELA " + SRD->RD_Y_EMCON
			EndIf
		EndIf
	EndIf
*/
	If cTipo # 'B'
		//--Array para Recibo Pre-Impresso
		nPos := Ascan(aLanca,{ |X| X[2] = cPd })
		If nPos == 0 .Or. !lAglutPd
			Aadd(aLanca,{cTipo,cPd,Desc_Paga,nHoras,nValor})
		Else
			aLanca[nPos,4] += nHoras
			aLanca[nPos,5] += nValor
		Endif
	Endif

	//--Array para o Recibo Pre-Impresso
	If cTipo = 'P'
		cArray := "aProve"
	Elseif cTipo = 'D'
		cArray := "aDesco"
	Elseif cTipo = 'B'
		cArray := "aBases"
	Endif

	nPos := Ascan(&cArray,{ |X| X[1] = cPd })
	If nPos == 0 .Or. !lAglutPd
		Aadd(&cArray,{cPd+" "+Desc_Paga,nHoras,nValor })
	Else
		&cArray[nPos,2] += nHoras
		&cArray[nPos,3] += nValor
	Endif
Return


/*/{Protheus.doc} fImpTeste
Testa impressao de Formulario Teste
@type Function
@author Natie
@since 29/11/2001
@version AP5
/*/

Static function fImpTeste(cString,nTipoRel)
	Local nArea := 0
	//--Comando para nao saltar folha apos o MsFlush.
	SetPgEject(.F.)

	// Descarrega teste de impressao
	MS_Flush()

	If !fInicia(cString,nTipRel)
		Return
	EndIf

	// Define o Li com a a linha de impressão correten para não saltar linhas no teste
	li := _Prow()

	If nTipoRel == 2
		@ LI,00 PSAY AvalImp(Limite)
	Endif

	nArea := Select("SRA")
	If nArea > 0
		DbSelectArea(nArea)
	EndIF

	//--> Só pergunta na primeira impressão!
	If Vez == 0
		Vez++
		lRetCanc	:= Pergunte("GPR30A",.T.)
	EndIf

Return Vez

/*/{Protheus.doc} fImpTeste
Inicializa parametros para impressao
@type Function
@author Natie
@since 04/12/2001
@version AP5
/*/

Static Function  fInicia(cString,nTipoRel)

	If LastKey() = 27 .Or. nLastKey = 27
		Return  .F.
	Endif

	If nTipoRel == 1 .OR. nTipoRel == 2
		SetDefault(aReturn,cString)
	Endif

	If LastKey() = 27 .OR. nLastKey = 27
		Return .F.
	Endif

Return .T.

/*/{Protheus.doc} RodaHtml
Retorna a data valida para a consulta do Terminal Consulta do Funcionário
@type Function
@author Ricardo Duarte
@since 13/08/2004
@version GPER030
/*/
Static Function fMontaDtTcf(cMesAno,nDia)

	Local dDataValida
	Default nDia := 0

	dDataValida := stod(right(cMesAno,4)+left(cMesAno,2)+"01")
	dDataValida := stod(right(cMesAno,4)+left(cMesAno,2)+strzero(f_UltDia(dDataValida),2))+nDia

return(dDataValida)

/*/{Protheus.doc} f030Roteiro
Selecionar o Roteiro
@type Function
@author Tatiane Matias
@since 28/02/2005
@version Generic
/*/
Static Function f030Roteiro(l1Elem, lTipoRet, lVExtras)

	Local cTitulo	:="Roteiro de Calculo"
	Local nFor		:= 0
	Local nElem		:= 0
	Local MvPar
	Local MvParDef	:=""
	Local MvRetor	:= ""

	Private aSit:={}
	l1Elem := If (l1Elem = Nil , .F. , .T.)

	DEFAULT lTipoRet 	:= .T.
	DEFAULT lVExtras	:= .T.

	cAlias := Alias() 					// Salva Alias Anterior

	IF lTipoRet
		MvPar:=&(Alltrim(ReadVar()))	// Carrega Nome da Variavel do Get em Questao
		mvRet:=Alltrim(ReadVar())		// Iguala Nome da Variavel ao Nome variavel de Retorno
	EndIF

	dbSelectArea("SRY")
	If dbSeek(cFilial)
		CursorWait()
		While !Eof() .And. SRY->RY_FILIAL == cFilial
			If !(SRY->RY_TIPO $ "3*4*G*J")
				Aadd(aSit, SRY->RY_CALCULO + " - " + Alltrim(SRY->RY_DESC))
				MvParDef += SRY->RY_CALCULO
			EndIf
			dbSkip()
		Enddo
		If lVExtras
			Aadd(aSit, "EXT - Valores Extras")
			MvParDef += "EXT"
		EndIf
		CursorArrow()
	Endif

	IF lTipoRet
		IF f_Opcoes(@MvPar,cTitulo,aSit,MvParDef,,,l1Elem, GetSx3Cache("RY_CALCULO","X3_TAMANHO"))  // Chama funcao f_Opcoes
			CursorWait()
			For nFor := 1 To Len( mVpar ) Step 3
				IF ( SubStr( mVpar , nFor , 3 ) # "***" )
					mvRetor += SubStr( mVpar , nFor , 3 )
				Endif
			Next nFor
			&MvRet := Mvretor
			CursorArrow()
		EndIF
	EndIF

	dbSelectArea(cAlias) // Retorna Alias

Return( IF( lTipoRet , .T. , MvParDef ) )

/*/{Protheus.doc} fRetFerFunc
Retornar as datas de periodo aquisitivo, data inicio e data fim das ferias de um funcionario.
@type Function
@author Alceu Pereira
@since 15/07/2011
@version Generico Localizacao Peru
/*/

Static Function fRetFerFunc(dDataPerAd, dDataPerAt ,dDataIniF , dDAtaFimF)

	Local cFil   := SRA->RA_FILIAL
	Local cMat   := SRA->RA_MAT
	Local aArea	 := GetArea()

	dbSelectArea("SRH")
	DbSetOrder( 1 )
	If dbSeek( cFil + cMat )
		While !Eof() .AND. SRH->RH_FILIAL == cFil .AND. SRH->RH_MAT == cMat
			If AllTrim(Str(Ano(SRH->RH_DATAINI))) == cAno .AND. SRH->RH_FILIAL == cFil .AND. SRH->RH_MAT == cMat
				dDataPerAd := SRH->RH_DATABAS
				dDataPerAt := SRH->RH_DBASEAT
				dDataIniF := SRH->RH_DATAINI
				dDAtaFimF := SRH->RH_DATAFIM
				Exit
			Endif
			dbSkip()
		EndDo
	Endif
	RestArea(aArea)
Return

/*/{Protheus.doc} fRDescAFP
Exibir descricao do tipo de AFP da tabela S004 para localização Peru.
@type Function
@author Alceu Pereira
@since 19/07/2011
@version Generico Localizacao Peru
/*/
Static Function fRDescAFP()

	Local nAux:= 1
	Local cDescAfp := ""
	Local aTabDescAFP := {}

	fCarrTab( @aTabDescAFP, "S004", Nil)

	IF ( len(aTabDescAFP) >= 1 )
		nAux := 1

		If cPaisLoc == "COL" .OR. cPaisLoc == "PER"
			While ( nAux <= len(aTabDescAFP) )
				If ( aTabDescAFP[nAux,1] == "S004" .And. aTabDescAFP[nAux,5] == SRA->RA_CODAFP )
					cDescAfp := Trim(aTabDescAFP[nAux,6])
					nAux := LEN(aTabDescAFP)
				Endif
				nAux++
			End
		Else
			cDescAfp := ' '
		EndIf
	Endif

Return cDescAfp

/*/{Protheus.doc} fAfasSub
Retorna quantidade de dias de afastamentos subsidiados pela Empresa.
@type Function
@author Alceu Pereira
@since 25/07/2011
@version Generico Localizacao Peru
/*/

Static Function fAfasSub(nDiasAfs)

	Local cFil   := SRA->RA_FILIAL
	Local cMat   := SRA->RA_MAT
	Local aArea	 := GetArea()

	dbSelectArea("SR8")
	DbSetOrder( 1 )
	If dbSeek( cFil + cMat )
		While !Eof() .AND. SR8->R8_FILIAL == cFil .AND. SR8->R8_MAT == cMat
			If cMes >= StrZero(Month(SR8->R8_DATAINI),2) .AND. cMes <= StrZero(Month(SR8->R8_DATAFIM),2) .AND. cAno = StrZero(Year(SR8->R8_DATAFIM),4)
				If SR8->R8_DPAGAR > 0
					nDiasAfs := nDiasAfs + SR8->R8_DPAGAR
				Endif
			Endif
			dbSkip()
		EndDo
	Endif
	RestArea(aArea)

Return nDiasAfs

/*/{Protheus.doc} fHorasFunc
Retorna quantidade de horas trabalhadas ou quantidade de horas extras feitas pelo funcionario.
@type Function
@author Alceu Pereira
@since 25/07/2011
@version Generico Localizacao Peru
/*/

Static Function fHorasFunc(lHorasExtras, nQuantHoras, Semana)

	Local aArea	        := GetArea()
	Local cChaveSem     := ""
	Local aVerbasLIQ    := {}
	Local nAux			:= 0
	Local aPerAberto    := {}
	Local aPerFechado   := {}
	Local aPerTodos     := {}

	If lHorasExtras == .T.

		fRetPerComp( 	cMes		  ,;	// Obrigatorio - Mes para localizar as informacoes
		cAno		  ,;	// Obrigatorio - Ano para localizar as informacoes
		xFilial("RCH"),;	// Opcional - Filial a Pesquisar
		,;		// Opcional - Filtro por Processo
		,;		// Opcional - Filtro por Roteiro
		@aPerAberto	  ,;	// Por Referencia - Array com os periodos Abertos
		@aPerFechado, ;		// Por Referencia - Array com os periodos Fechados
		@aPerTodos    ;		// Por Referencia - Array com os periodos Abertos e Fechados em Ordem Crescente
		)
	Endif

	nQuantHoras := 0

	cChaveSem := StrZero(Year(dDataRef),4)+StrZero(Month(dDataRef),2)

	dbSelectArea( "RCF" )
	DbSetOrder( 1 )

	If RCF->( dbSeek( xFilial("RCF") + cChaveSem,.T.)) .AND. lHorasExtras == .F.
		nQuantHoras := RCF->RCF_HRSTRA + RCF->RCF_HRSDSR
	Else
		aVerbasLIQ := RetornaVerbasFunc(SRA->RA_FILIAL, SRA->RA_MAT, ,fGetCalcRot("1"), , aPerAberto, aPerFechado )
		For nAux:= 1 to len(aVerbasLIQ)
			If (PosSrv(aVerbasLIQ[nAux,3], SRA->RA_FILIAL, "RV_HE")) == "S"
				If PosSrv( aVerbasLIQ[nAux,3], SRA->RA_FILIAL, "RV_TIPOCOD" ) $ "1*3"
					nQuantHoras += aVerbasLIQ[nAux,6]
				Elseif PosSrv( aVerbasLIQ[nAux,3], SRA->RA_FILIAL, "RV_TIPOCOD" ) $ "2*4"
					nQuantHoras -= aVerbasLIQ[nAux,6]
				Endif
			Endif
		Next
	Endif

	RestArea (aArea)
Return nQuantHoras


/*/{Protheus.doc} fImprPDF
Impressão do Recibo em PDF
@type Function
@version Generic
/*/
Static Function fImprPDF(lPDFEmail,cPath)
	Local nConta 	:= 0
	Local cFile		:= ""
	Local cEmail	:= ""
	Local cIdentUni	:= ""

	Local nCnt	  := 0

	Local aProv		:= {}
	Local aDesc		:= {}

	Local i		  := 0
	Local nConta  := 0
	Local nContr  := 0
	Local nContrT := 0
	Local nQtdFol := 0
	nLinhas       := 15   // Numero de Linhas do Miolo do Recibo

	If _cont >= 3000
		_cont:= 0
		_oPrint:StartPage()   // Inicia uma nova página
	Else
		_oPrint:Say( 1700, 0010, replicate("-", 250), _oFont10) //
	Endif

	// Imprime Cabecalho
	fCabecGrf()
	nLin := 470

	//¦+--------------------------------------------------------------+¦
	//¦¦Ordena Array de Lançamentos P/D | Lucas Nogueira @  11/11/2018¦¦
	//¦+--------------------------------------------------------------+¦
	For i := 1 To Len(aLanca)
		If aLanca[i][1] == "P"
			aAdd(aProv, {} )
			aProv[Len(aProv)] := aLanca[i]
		Else
			aAdd(aDesc, {} )
			aDesc[Len(aDesc)] := aLanca[i]
		Endif
	Next i

	aLanca := {}

	For i := 1 To Len(aProv)
		aAdd(aLanca, {} )
		aLanca[Len(aLanca)] := aProv[i]
	Next i

	For i := 1 To Len(aDesc)
		aAdd(aLanca, {} )
		aLanca[Len(aLanca)] := aDesc[i]
	Next i

	//de acordo com a quantidade de linhas, quantidade de folhas de holerite
	If Len(aLanca) <= 15
		nQtdFol := 1
	ElseIf Len(aLanca) > 15 .And. Len(aLanca) <= 30
		nQtdFol := 2
	ElseIf Len(aLanca) > 30 .And. Len(aLanca)  <= 57
		nQtdFol := 3
	ElseIf Len(aLanca) > 57 .And. Len(aLanca)  <= 78
		nQtdFol := 4
	EndIf

	//Lançamentos
	If nQtdFol == 1
		For nConta = 1 To Len(aLanca)
			fLanUni(nConta)
		Next nConta

	ElseIf nQtdFol == 2
		For nConta = 1 To Len(aLanca)
			fLanUni(nConta)
			nContr ++
			//¦+---------------------------------------------------------+¦
			//¦¦Ajuste da Página    	   | Lucas Nogueira @  10/11/2018¦¦
			//¦+---------------------------------------------------------+¦
			If nContr == 15	//21 quebra a página
				nContr := 0
				_oPrint:Line(_cont+0390, 0250, _cont+1380, 0250)
				_oPrint:Line(_cont+0390, 1135, _cont+1380, 1135)

				_oPrint:Say(_cont+nLin+025, 0600, " *** CONTINUA *** ", _oFont14)

				If _cont >= 1800
					_cont:= 0
					_oPrint:EndPage()
					_oPrint:StartPage()
				Else
					_cont := 1745
				Endif

				fCabecGrf()
				nLin := 470
			Endif
		Next nConta

	ElseIf nQtdFol == 3
		For nConta = 1 To Len(aLanca)
			fLanUni(nConta)
			nContr ++
			If nContr == 21 .Or. nContr == 42	//quebra a página
				nContr := 0
				_oPrint:Line(_cont+0390, 0250, _cont+1380, 0250)
				_oPrint:Line(_cont+0390, 1135, _cont+1380, 1135)

				_oPrint:Say(_cont+nLin+025, 0600, " *** CONTINUA *** ", _oFont14)

				_cont := 1745

				nCnt++
				If nCnt == 2
					_oPrint:EndPage()
					_oPrint:StartPage()
					_cont:= 0
				Endif

				fCabecGrf()
				nLin := 470
			Endif
		Next nConta

	ElseIf nQtdFol == 4
		For nConta = 1 To Len(aLanca)
			fLanUni(nConta)
			nContr ++
			If nContr == 21 .Or. nContr == 42 .Or. nContr == 57	//quebra a página
				//nContr:=0
				_oPrint:Line(_cont+0390, 0250, _cont+1380, 0250)
				_oPrint:Line(_cont+0390, 1135, _cont+1380, 1135)

				_oPrint:Say(_cont+nLin+025, 0600, " *** CONTINUA *** ", _oFont14)

				_cont := 1730

				nCnt++
				If nCnt == 2 .Or. nCnt == 4
					_oPrint:EndPage()
					_oPrint:StartPage()
					_cont:= 0
				Endif

				fCabecGrf()
				nLin := 470
			Endif
		Next nConta

	EndIf

	// Imprime o rodapé
	_cont += 35
	fRodaUni()
	_cont += 1730

	If _cont  >= 3000
		_oPrint:EndPage()     // Finaliza a página
	EndIf

Return Nil

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o    ³fImpGrf   ³ Autor ³ Lúcia Valéria         ³ Data ³ 27.05.07 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Sintaxe   ³ fCabecGrf()                                                ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Função para impressão do Cabeçalho                                     ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß*/
Static Function fCabecGrf()

	Local cCodFunc		:= ""		//-- codigo da Funcao do funcionario
	Local cDescFunc		:= ""		//-- Descricao da Funcao do Funcionario

	/*ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	³ Carrega Funcao do Funcion. de acordo com a Dt Referencia     ³
	ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ*/
	fBuscaFunc(dDataRef, @cCodFunc, @cDescFunc   )

	//Cabeçalho do holerite dados da empresa
	_cont+=50                                                    //Margem Para Cabecalho
	//Linha 1
	_oPrint:Say(_cont+010, 0600, Desc_Fil, _oFont14) //Nome da Empresa  /_aEmp[1]
	//Linha 2
	_oPrint:Say(_cont+080, 0600, Desc_End + "  -  "+ Desc_Bairro , _oFont10) //Endereço da Empresa // _aEmp[2]
	//Linha 3
	_oPrint:Say(_cont+120, 0600, Desc_Cid + " - " + Desc_Est + "  -  "+ Desc_CEP , _oFont10) //Cidade da empresa / /_aEmp[3]
	//Linha 4
	_oPrint:Say(_cont+160, 0600, Desc_CGC , _oFont10) //Cnpj // _aEmp[5]
	//Linha 5
	_oPrint:Say(_cont+160, 1100, Desc_IE, _oFont10) //IE _aEmp[6]

	//Imagem logo do lado Esquerdo do holerite
	_oPrint:SayBitmap(_cont+010, 0120, _cfig, 380, 160)

	//Titulo do Holerite e a base do mes
	//Quadrante Box que ira receber o titulo
	_oPrint:Box(_cont+0220, 0100, _cont+0290, 2300)
	//Titulo
	_reltipo := OemToAnsi(" Recibo de Pagamento de Salário")
	_oPrint:Say(_cont+0230, 0600,_reltipo, _oFont16)
	//Mes e ano de Base de Pagamento
	_oPrint:Say(_cont+0230, 1850,MesExtenso(MONTH(dDataRef))+"/"+STR(YEAR(dDataRef),4), _oFont16)

	//Dados do Funcionario
	//Quadrante Box que ira receber os dados do funcionarios
	_oPrint:Box(_cont+0300, 0100, _cont+0380, 2300)

	//Linha 1
	_oPrint:Say(_cont+0310, 0115, OemToAnsi(SRA->RA_MAT) + " - " + SRA->RA_NOME, _oFont8b)//codigo
	_oPrint:Say(_cont+0310, 1000, "CPF: " + SRA->RA_CIC, _oFont8b)//codigo // FLUIG - 383098 e 375152 - INCLUIR CPF - GUILHERME
	_oPrint:Say(_cont+0310, 1405, OemToAnsi("Centro de Custo: ") , _oFont8)//cto Custo
	_oPrint:Say(_cont+0310, 1650, Alltrim(SRA->RA_CC) + " - " + Alltrim(Posicione("CTT",1,xFilial("CTT") + SRA->RA_CC,"CTT_DESC01")), _oFont8)//cto custo

	//Linha 2
	_oPrint:Say(_cont+0345, 0115, OemToAnsi("Função:")                    , _oFont8)//Titulo Funcao
	_oPrint:Say(_cont+0345, 0235, Alltrim(cCodFunc) +" - "+ Alltrim(Posicione("SRJ",1,xFilial("SRJ") + SRA->RA_CODFUNC, "RJ_DESC") ), _oFont8)//Campo Codigo da Funcao
	_oPrint:Say(_cont+0345, 1000,  "C.B.O.: " + Alltrim(Posicione("SRJ",1,xFilial("SRJ") + SRA->RA_CODFUNC, "RJ_CODCBO") ), _oFont8)//Campo descricao da Funcao
	_oPrint:Say(_cont+0345, 1405, OemToAnsi("Data Admissão")          , _oFont8)//Titulo Data Admissao
	_oPrint:Say(_cont+0345, 1650, dtoc(SRA->RA_ADMISSA) , _oFont8)//Campo Data Admissao

	//Titulo dos Dados do Holerite

	//Box ( < Linha inicial>, < Coluna inicial>, < Linha final	>, < Coluna final>, [ uParam5] )
	//_oPrint:Box(_cont+1280, 0100, _cont+1380, 2300)
	//Quadrante Box que ira receber os dados do Holerite
	_oPrint:Box(_cont+0390, 0100, _cont+1415, 2300)

	//Divisao Linha e Titulo Codigo
	_oPrint:Line(_cont+0450, 0100, _cont+0450, 2300)
	_oPrint:Say(_cont+0400, 0115, OemToAnsi("Cód")            , _oFont8b)

	//Divisao Linha e Titulo Descricao
	_oPrint:Line(_cont+0390, 0250, _cont+1150, 0250)
	_oPrint:Say(_cont+0400, 0650, OemToAnsi("Descrição")    , _oFont8b)

	//Divisao Linha e Titulo Referencia
	_oPrint:Line(_cont+0390, 1135, _cont+1150, 1135)
	_oPrint:Say(_cont+0400, 1150, OemToAnsi("Referência")    , _oFont8b)

	//Divisao Linha e Titulo Vencimento
	_oPrint:Line(_cont+0390, 1300, _cont+1415, 1300)
	_oPrint:Say(_cont+0400, 1450, "Vencimentos", _oFont8b)

	//Divisao Linha e Titulo Descontos
	_oPrint:Line(_cont+0390, 1800, _cont+1415, 1800)
	_oPrint:Say(_cont+0400, 1950, "Descontos"   , _oFont8b)

Return()


/*/{Protheus.doc} fLanUni
//TODO Descrição auto-gerada.
@version 1.0
@param nConta, numeric, description
@type function
/*/
Static Function fLanUni(nConta)

	Local	cString := Transform(aLanca[nConta,5],cPict3)
	Local 	nCol    := If(aLanca[nConta,1]="P",1600,If(aLanca[nConta,1]="D",2130,27))

	_oPrint:Say(_cont+nLin, 0115, aLanca[nConta,2], _oFont8)
	_oPrint:Say(_cont+nLin, 0300, aLanca[nConta,3], _oFont8)
	If aLanca[nConta,1] # "B"        // So Imprime se nao for base
		//		_oPrint:Say(_cont+nLin, 1170, TRANSFORM(aLanca[nConta,4],"999.99"), _oFont8)
		If aLanca[nConta,4] < 10.00
			_oPrint:Say(_cont+nLin, 1176, PadL(AllTrim(Transform(aLanca[nConta,4],"9.99")),6), _oFont8)
		ElseIf aLanca[nConta,4] < 100.00
			_oPrint:Say(_cont+nLin, 1172, PadL(AllTrim(Transform(aLanca[nConta,4],"99.99")),6), _oFont8)
		Else
			_oPrint:Say(_cont+nLin, 1168, PadL(AllTrim(Transform(aLanca[nConta,4],"999.99")),6), _oFont8)
		EndIf

	Endif
	//alinhamento
	If nCol == 1600	//provento
		Do Case
		Case aLanca[nConta,5] < 1.00
			_oPrint:Say(_cont+nLin, nCol+25, cString, _oFont8)
		Case aLanca[nConta,5] < 10.00
			_oPrint:Say(_cont+nLin, nCol+21, cString, _oFont8)
		Case aLanca[nConta,5] < 100.00
			_oPrint:Say(_cont+nLin, nCol+16, cString, _oFont8)
		Case aLanca[nConta,5] < 1000.00
			_oPrint:Say(_cont+nLin, nCol+12, cString, _oFont8)
		Case aLanca[nConta,5] < 10000.00
			_oPrint:Say(_cont+nLin, nCol+04, cString, _oFont8)
		Case aLanca[nConta,5] < 100000.00
			_oPrint:Say(_cont+nLin, nCol+04, cString, _oFont8)
		OtherWise
			_oPrint:Say(_cont+nLin, nCol   , cString, _oFont8)
		EndCase
	Else	//desconto
		Do Case
		Case aLanca[nConta,5] < 1.00
			_oPrint:Say(_cont+nLin, nCol+25, cString, _oFont8)
		Case aLanca[nConta,5] < 10.00
			_oPrint:Say(_cont+nLin, nCol+21, cString, _oFont8)
		Case aLanca[nConta,5] < 100.00
			_oPrint:Say(_cont+nLin, nCol+16, cString, _oFont8)
		Case aLanca[nConta,5] < 1000.00
			_oPrint:Say(_cont+nLin, nCol+12, cString, _oFont8)
		Case aLanca[nConta,5] < 10000.00
			_oPrint:Say(_cont+nLin, nCol+08, cString, _oFont8)
		Case aLanca[nConta,5] < 100000.00
			_oPrint:Say(_cont+nLin, nCol+04, cString, _oFont8)
		OtherWise
			_oPrint:Say(_cont+nLin, nCol   , cString, _oFont8)
		EndCase
	EndIf
	nLin += 40

Return


/*/{Protheus.doc} fRodaUni
//TODO Descrição auto-gerada.
@since 05/02/2020
@version 1.0

@type function
/*/
Static Function fRodaUni()

	Local nPosArray := aScan(aLanca,{|x| x[2] == "419"})

	If nPosArray > 0
		nFaixaIRRF := aLanca[nPosArray,4]
	Else
		nFaixaIRRF := 0
	EndIf

	//Divisao linha para mensagens e os totais
	_oPrint:Line(_cont+1115, 0100, _cont+1115, 2300)
	_oPrint:Say(_cont+1125, 1450, "Total de Vencimentos", _oFont8b)
	_oPrint:Say(_cont+1125, 1950, "Total de Descontos"   , _oFont8b)

	//Divisao linha para Valor Liquido
	//_oPrint:Line(_cont+1180,1300, _cont+1180, 2300)
	_oPrint:Say(_cont+1240, 1445, "Valor Líquido      ==>"   , _oFont10b)

	//Quadrante BOX e Imprime os Titulos dos Totais de Base
	_oPrint:Box(_cont+1280, 0100, _cont+1380, 2300)
	_oPrint:Say(_cont+1295, 0150, OemToAnsi("Salário-Base")   , _oFont8b)

	//Raphael Ferreira - Oficina5 - Chamado - 105979
	IF SRA->RA_FILIAL <> '16'
		_oPrint:Say(_cont+1295, 0400, OemToAnsi("Base Marg. Consig")   , _oFont8b) // Não aparecer a Margem consignado filial 16 - Guilherme - FLUIG 375152
	ENDIF
	_oPrint:Say(_cont+1295, 0750, OemToAnsi("Sal. Contr. INSS")   , _oFont8b)

	_oPrint:Say(_cont+1295, 1000, OemToAnsi("Base Cálc. FGTS")   , _oFont8b)
	_oPrint:Say(_cont+1295, 1310, OemToAnsi("F.G.T.S. do Mês")   , _oFont8b)
	_oPrint:Say(_cont+1295, 1585, OemToAnsi("Base Cálc. IRRF")   , _oFont8b)
	_oPrint:Say(_cont+1295, 2010, OemToAnsi("Faixa IRRF")   , _oFont8b)

	// Impressao das Mensagens
	_oPrint:Say(_cont+1115, 0115, DESC_MSG1, _oFont8) //Mensagem 1
	_oPrint:Say(_cont+1155, 0115, DESC_MSG2, _oFont8) //Mensagem 2
	_oPrint:Say(_cont+1195, 0115, DESC_MSG3, _oFont8) //Mensagem 3

	If Month(dDataRef) == Month(SRA->RA_NASC)
		_oPrint:Say(_cont+1200, 0115, "F E L I Z   A N I V E R S Á R I O ! !", _oFont8)
	EndIf

	//Mensagens da conta corrente
	If SRA->RA_BCDEPSAL # SPACE(8)
		//		DbSelectArea("SA6")
		//		SA6->(DbSetOrder(1))
		//		If SA6->(DbSeek(xFilial("SA6")+SRA->RA_BCDEPSA))
		//			Desc_Bco := AllTrim(SA6->A6_NREDUZ)
		//			_oPrint:Say(_cont+1240, 0115, "Crédito: "+Transform(SRA->RA_BCDEPSAL,"@R 999/99999") +" - " +Desc_Bco+" Conta: " + SRA->RA_CTDEPSAL, _oFont8)
		//		EndIf

		//		DbSelectArea("SA6")
		//		SA6->(DbCloseArea())
		If SubStr(SRA->RA_MAT,1,1) $ "0"
			 Desc_Bco := "  "
			_oPrint:Say(_cont+1240, 0145, "Credito: "+Transform(Sra->Ra_BcDepSal,"@R 999/99999") +" - " +DESC_BCO+" Conta: " + SRA->RA_CTDEPSAL, _oFont8)
		EndIf
	EndIf

	//Impressao dos Totais Do campo Vencimentos
	If TOTVENC <= 9.99
		_oPrint:Say(_cont+1165, 1620,TRANSFORM(TOTVENC,cPict1), _oFont8)
	Endif
	If TOTVENC <= 99.99 .and. TOTVENC > 9.99
		_oPrint:Say(_cont+1165, 1616,TRANSFORM(TOTVENC,cPict1), _oFont8)
	Endif
	If TOTVENC <= 999.99 .and. TOTVENC > 99.99
		_oPrint:Say(_cont+1165, 1612,TRANSFORM(TOTVENC,cPict1), _oFont8)
	Endif
	If TOTVENC <= 9999.99 .and. TOTVENC > 999.99
		_oPrint:Say(_cont+1165, 1608,TRANSFORM(TOTVENC,cPict1), _oFont8)
	Endif
	If TOTVENC <= 99999.99 .and. TOTVENC > 9999.99
		_oPrint:Say(_cont+1165, 1604,TRANSFORM(TOTVENC,cPict1), _oFont8)
	Endif

	//Impressao dos Totais Do campo Descontos
	If TOTDESC <= 9.99
		_oPrint:Say(_cont+1165, 2150,TRANSFORM(TOTDESC,cPict1), _oFont8)
	Endif
	If TOTDESC <= 99.99 .and. TOTDESC > 9.99
		_oPrint:Say(_cont+1165, 2146,TRANSFORM(TOTDESC,cPict1), _oFont8)
	Endif
	If TOTDESC <= 999.99 .and. TOTDESC > 99.99
		_oPrint:Say(_cont+1165, 2142,TRANSFORM(TOTDESC,cPict1), _oFont8)
	Endif
	If TOTDESC <= 9999.99 .and. TOTDESC > 999.99
		_oPrint:Say(_cont+1165, 2138,TRANSFORM(TOTDESC,cPict1), _oFont8)
	Endif
	If TOTDESC <= 99999.99 .and. TOTDESC > 9999.99
		_oPrint:Say(_cont+1165, 2134,TRANSFORM(TOTDESC,cPict1), _oFont8)
	Endif

	//Impressao dos Totais do Campo Valor Liquido
	If (TOTVENC-TOTDESC) <= 9.99
		_oPrint:Say(_cont+1240, 2150,TRANSFORM((TOTVENC-TOTDESC),cPict1), _oFont8)
	Endif
	If (TOTVENC-TOTDESC) <= 99.99 .and. (TOTVENC-TOTDESC) > 9.99
		_oPrint:Say(_cont+1240, 2146,TRANSFORM((TOTVENC-TOTDESC),cPict1), _oFont8)
	Endif
	If (TOTVENC-TOTDESC) <= 999.99 .and. (TOTVENC-TOTDESC) > 99.99
		_oPrint:Say(_cont+1240, 2142,TRANSFORM((TOTVENC-TOTDESC),cPict1), _oFont8)
	Endif
	If (TOTVENC-TOTDESC) <= 9999.99 .and. (TOTVENC-TOTDESC) > 999.99
		_oPrint:Say(_cont+1240, 2138,TRANSFORM((TOTVENC-TOTDESC),cPict1), _oFont8)
	Endif
	If (TOTVENC-TOTDESC) <= 99999.99 .and. (TOTVENC-TOTDESC) > 9999.99
		_oPrint:Say(_cont+1240, 2134,TRANSFORM((TOTVENC-TOTDESC),cPict1), _oFont8)
	Endif

	//Imprime Salario Base
	nValSal := SRA->RA_SALARIO
	_oPrint:Say(_cont+1325, 0150, Transform(nValSal,cPict2) , _oFont8)

	//Raphael Ferreira - Oficina5 - Chamado - 105979
	IF SRA->RA_FILIAL <> '16'	
		If nVbMgConsig > 0
			_oPrint:Say(_cont+1325, 0430, Transform(nVbMgConsig,cPict2) , _oFont8)
		Else
			_oPrint:Say(_cont+1325, 0430, "Sem Margem", _oFont8)
		EndIf
	ENDIF
	

	If Esc = 1  // Bases de Adiantamento
		If cBaseAux = "S" .And. nBaseIr # 0
			_oPrint:Say(_cont+1325, 1600, TRANSFORM(nBaseIr,cPict1) , _oFont8)
		Endif
	ElseIf Esc = 2 .Or. Esc = 4  // Bases de Folha e 13o. 2o.Parc.
		If cBaseAux = "S"
			//Raphael Ferreira - Oficina5 - Chamado - 105979
			_oPrint:Say(_cont+1325, 0775,Transform(nAteLim,cPict1) , _oFont8)

			If nBaseFgts # 0
				_oPrint:Say(_cont+1325, 1000, TRANSFORM(nBaseFgts,cPict1) , _oFont8)
			Endif
			If nFgts # 0
				_oPrint:Say(_cont+1325, 1300,TRANSFORM(nFgts,cPict2) , _oFont8)
			Endif
			If nBaseIr # 0
				_oPrint:Say(_cont+1325, 1600,TRANSFORM(nBaseIr+nBaseIrfE,cPict1) , _oFont8)
			Endif
			_oPrint:Say(_cont+1325, 2000, Transform(nFaixaIRRF,cPict1) , _oFont8)
		Endif
	ElseIf Esc = 3 // Bases de FGTS e FGTS Depositado da 1¦ Parcela
		If cBaseAux = "S"
			If nBaseFgts # 0
				_oPrint:Say(_cont+1325, 0800,TRANSFORM(nBaseFgts,cPict1), _oFont8)
			Endif
			If nFgts # 0
				_oPrint:Say(_cont+1325, 1300, TRANSFORM(nFgts,cPict2) , _oFont8)
			Endif
		Endif
	Endif

	//Quadrante Box e Imprime a Mensagem de Creito automatico.
	_oPrint:Box(_cont+1390, 0100, _cont+1500, 2300)
	_oPrint:Say(_cont+1395, 0150, "***************** - CRÉDITO AUTOMÁTICO EM CONTA CORRENTE - ************"   , _oFont8b)
	_oPrint:Say(_cont+1450, 0150, "_____ / _____ / ________ "   , _oFont10)
	_oPrint:Say(_cont+1450, 1000, "Assinatura:  " + Replicate("_", 40)  , _oFont10)

Return()

/*/{Protheus.doc} fEnvPDFEmail
Prepara o envio do email com o PDF Anexado
@type Function
@author Emerson Campos
@since 30/06/2015
@version Generic
/*/

Static Function fEnvPDFEmail(cPath)
	Local nI			:= 0
	Local cSubject		:= STR0196		//" Demosntrativo de Pagamento "
	Local cMsg			:= ""
	Local cPathFile		:= ""
	Local cMsgErr1		:= ""	//Erro envio
	Local cMsgErr2		:= ""	//Sem email cadastrado
	Local cMsgErr3		:= ""	//Opção não envia email habilitada
	Local lReturn		:= .F.
	Local cFile			:= ""
	Private cEmailDP	:= NIL
	Private cMailConta	:= NIL
	Private cMailServer	:= NIL
	Private cMailSenha	:= NIL

	// Busca parametros
	cMailConta		:=If(cMailConta == NIL,GETMV("MV_EMCONTA"),cMailConta)             //Conta utilizada p/envio do email
	cMailServer	:=If(cMailServer == NIL,GETMV("MV_RELSERV"),cMailServer)           //Server
	cMailSenha		:=If(cMailSenha == NIL,GETMV("MV_EMSENHA"),cMailSenha)
	cEmailDP		:=If(cEmailDP == NIL,GETMV("MV_GPEMAIL"),cEmailDP)					//Email do responsável de Depto Pessoal

	// Verifica se existe o SMTP Server
	If 	Empty(cMailServer)
		Help(" ",1,"SEMSMTP")//"O Servidor de SMTP nao foi configurado !!!" ,"Atencao"
		Return(.F.)
	EndIf

	// Verifica se existe a CONTA
	If 	Empty(cMailConta)
		Help(" ",1,"SEMCONTA")//"A Conta do email nao foi configurado !!!" ,"Atencao"
		Return(.F.)
	EndIf

	// Verifica se existe a Senha
	If 	Empty(cMailSenha)
		Help(" ",1,"SEMSENHA")	//"A Senha do email nao foi configurado !!!" ,"Atencao"
		Return(.F.)
	EndIf

	For nI := 1 To Len(aEmail)
		cFile		:= aEmail[nI,2]+aEmail[nI,3]+aEmail[nI,4]+aEmail[nI,5]+"RecPag"
		cPathFile	:= cPathTmp + "totvsprinter\" + cFile
		CpyT2S( cPathFile+".pdf", "/spool/",.F.)
		If File(cPathFile+".pdf")
			fErase(cPathFile+".pdf")
		EndIf
		cPathFile	:= "spool\" + cFile

		If aEmail[nI,1,3]
			If ! Empty(AllTrim(aEmail[nI,1,2]))
				cSubject	:= STR0196 + " - " + MesExtenso(MONTH(SToD(aEmail[nI,4])))+"/"+STR(YEAR(StoD(aEmail[nI,4])),4)

				cMsg := STR0193+chr(13)+ "  " + aEmail[nI,1,1]+","+chr(10)	//"Sr.(a)"
				cMsg += chr(13)+chr(10)
				cMsg += chr(13)+chr(10)
				cMsg += STR0194+" "+MesExtenso(MONTH(SToD(aEmail[nI,4])))+"/"+STR(YEAR(StoD(aEmail[nI,4])),4)+chr(13)+chr(10) //"Segue em anexo o seu demonstrativo referente a:" Maio/2015
				cMsg += chr(13)+chr(10)
				cMsg += chr(13)+chr(10)
				cMsg += STR0195+chr(13)+chr(10)  //"Atenciosamente,"

				// verifico se o conteúdo do Parâmetro está diferente do diretório da SmartPath
				// Pois a Função TMailMessage só aceita diretórios que estejam dentro da SmartPath.
				// Faço uma cópia do Relatório para o SmartPath para não ter problema ao Enviar o PDF por E-mail
				lReturn := (alltrim(LOWER(cPath)) $ "\spool\")
				If !lReturn
					CpyT2S(cPathFile+".pdf", "\spool\", .F.)
					cPathFile := "\spool\" + cFile
				Endif

				// Envia e-mail p/funcionario
				If !GPEMail(cSubject,cMsg,aEmail[nI,1,2],{cPathFile+".pdf"})
					//Falha no envio
					cMsgErr1	+= "     " + aEmail[nI,2] + " - " + aEmail[nI,3] + " - " + aEmail[nI,1,1] + chr(13) + chr(10)
				Else
					lEnvioOK := .T.
				EndIf
			Else
				//Sem Email cadastrado
				cMsgErr2	+= "     " + aEmail[nI,2] + " - " + aEmail[nI,3] + " - " + aEmail[nI,1,1] + chr(13) + chr(10)
			EndIf
		Else
			//Marcado para não receber o email
			cMsgErr3	+= "     " + aEmail[nI,2] + " - " + aEmail[nI,3] + " - " + aEmail[nI,1,1] + chr(13) + chr(10)
		EndIf

		If File(cPathFile+".pdf")
			fErase(cPathFile+".pdf")
		EndIf

		If File(cPathTmp+cFile+".rel")
			fErase(cPathTmp+cFile+".rel")
		EndIf
	Next nI

	If ! Empty(cEmailDP)
		If ! Empty(cMsgErr1) .OR. ! Empty(cMsgErr2)  .OR. ! Empty(cMsgErr2)
			cSubject	:= STR0201 //"Erros ocorridos durante o envio dos demonstrativos de pagamento"
			cMsg := STR0193+"," + chr(13) + chr(10)	//"Sr.(a)"
			cMsg += chr(13) + chr(10)
			cMsg += chr(13) + chr(10)
			cMsg += STR0197 + chr(13) + chr(10) //"Ocorreram alguns erros durante o envio de demonstrativo de pagamento por email, veja a relação abaixo:
			cMsg += chr(13) + chr(10)
			cMsg += chr(13) + chr(10)
			If ! Empty(cMsgErr1)
				cMsg += "- "+ STR0198 +chr(13)+chr(10)	//- Erro durante o envio da mensagem
				cMsg += cMsgErr1 + chr(13) + chr(10)
				cMsg += chr(13) + chr(10)
				cMsg += chr(13) + chr(10)
			EndIf
			If ! Empty(cMsgErr2)
				cMsg += "- "+ STR0199 +chr(13)+chr(10)	//- Falta de endereço de email cadastrado para o envio:
				cMsg += cMsgErr2 + chr(13) + chr(10)
				cMsg += chr(13) + chr(10)
				cMsg += chr(13) + chr(10)
			EndIf
			If ! Empty(cMsgErr3)
				cMsg += "- "+ STR0200 +chr(13)+chr(10)	//- Marcado a opção de não receber email no caastro do funcionário:
				cMsg += cMsgErr3 + chr(13) + chr(10)
				cMsg += chr(13) + chr(10)
				cMsg += chr(13) + chr(10)
			EndIf
			cMsg += STR0195+chr(13)+chr(10)  //"Atenciosamente,"

			GPEMail(cSubject,cMsg,cEmailDP)
		EndIf
	EndIf
Return Nil


/*/{Protheus.doc} limpArqPdf
Limpar os arquivos PDF que são gerados pelo schedule
@type Function
@author Emerson Campos
@since 30/06/2015
@version Generic
/*/
Static Function limpArqPdf()
	Local cPath	:= Lower(supergetmv("MV_RELT",,"\spool\"))
	AEVAL(DIRECTORY(cPath+"*RecPag.pdf"), { |aFile| FERASE(cPath+aFile[1]) })
Return

