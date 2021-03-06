-------------------------------------------------------------------------
--
-- I M P L E M E N T A   O   P A R C I A L  D O  M I P S   (nov/2010)
--
--  ImPoRtAnTe :   VERSO  SEM MULTIPLICAO/DIVISO
--
--  Professores     Fernando Moraes / Ney Calazans
--
--  ==> The top-level processor entity is MRstd
--  21/06/2010 - Bug corrigido no mux que gera op1 - agora recebe npc e
--		no pc.
--  17/11/2010 (Ney) - Bugs corrigidos:
--	1 - Decodificao das instrues BGEZ e BLEZ estava incompleta
--		Modificadas linhas 395 e 396 abaixo
--	2 - Definio de que linhas escolhem o registrador a ser escrito
--	nas instrues de deslocamento (SSLL, SLLV, SSRA, SRAV, SSRL e SRLV)
--		Acrescentadas linhas 325 a 327 abaixo
-------------------------------------------------------------------------

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- package with basic types
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.Std_Logic_1164.all;

package p_MRstd is  
    
    -- inst_type defines the instructions decodeable by the control unit
    type inst_type is  
            ( ADDU, SUBU, AAND, OOR, XXOR, NNOR, SSLL, SLLV, SSRA, SRAV, SSRL, SRLV,
            ADDIU, ANDI, ORI, XORI, LUI, LBU, LW, SB, SW, SLT, SLTU, SLTI,
            SLTIU, BEQ, BGEZ, BLEZ, BNE, J, JAL, JALR, JR, NOP, invalid_instruction);
 
    type microinstruction is record
            --CY1:   std_logic;       -- command of the first stage
            --CY2:   std_logic;       --    "    of the second stage
            --walu:  std_logic;       --    "    of the third stage
            --wmdr:  std_logic;       --    "    of the fourth stage
            --wpc:   std_logic;       -- PC write enable
            wreg:  std_logic;       -- register bank write enable
            cem:   std_logic;       -- Chip enable and R_W controls
            rw:    std_logic;
            bw:    std_logic;       -- Byte-word control (mem write only)
            i:     inst_type;       -- operation specification
    end record;
	 
	 type BI_CONT is record
		ir:	std_logic_vector(31 downto 0);
		npc: 	std_logic_vector(31 downto 0);
		i: 	inst_type;
		wreg: std_logic;
		rw: 	std_logic;
		bw: 	std_logic;
		cem: 	std_logic;
		inst_branch:	std_logic;           
		inst_grupo1: 	std_logic;
		inst_grupoI: 	std_logic;
	 end record;
	 
	 type DI_CONT is record
		ir:	std_logic_vector(31 downto 0);
		npc: 	std_logic_vector(31 downto 0);
		i: 	inst_type;
		wreg: std_logic;
		rw: 	std_logic;
		bw: 	std_logic;
		cem: 	std_logic;
		inst_branch:	std_logic;           
		inst_grupo1: 	std_logic;
		inst_grupoI: 	std_logic;
	 end record;
	 
	 type EX_CONT is record
		ir:	std_logic_vector(31 downto 0);
		npc: 	std_logic_vector(31 downto 0);
		i: 	inst_type;
		wreg: std_logic;
		rw: 	std_logic;
		bw: 	std_logic;
		cem: 	std_logic;
		inst_branch:	std_logic;           
		inst_grupo1: 	std_logic;
		inst_grupoI: 	std_logic;
		adS : std_logic_vector(4 downto 0);
		IMED : std_logic_vector(31 downto 0);
		RA : std_logic_vector(31 downto 0);
		RB : std_logic_vector(31 downto 0);
	 end record;
	 
	 type ME_CONT is record
		ir:	std_logic_vector(31 downto 0);
		npc:	std_logic_vector(31 downto 0);
		i:	inst_type;
		wreg:	std_logic;
		rw: 	std_logic;
		bw: 	std_logic;
		cem: 	std_logic;
		inst_branch:	std_logic;           
		inst_grupo1: 	std_logic;
		inst_grupoI: 	std_logic;
		adS :	std_logic_vector(4 downto 0);
		IMED :	std_logic_vector(31 downto 0);
		RA :	std_logic_vector(31 downto 0);
		RB :	std_logic_vector(31 downto 0);
		RALU :	std_logic_vector(31 downto 0);
		salta : 	std_logic;
		adD : std_logic_vector(4 downto 0);
	 end record;
	 
	 type WB_CONT is record
		ir:	std_logic_vector(31 downto 0);
		npc:	std_logic_vector(31 downto 0);
		i:	inst_type;
		wreg:	std_logic;
		rw: 	std_logic;
		bw: 	std_logic;
		cem: 	std_logic;
		inst_branch:	std_logic;           
		inst_grupo1: 	std_logic;
		inst_grupoI: 	std_logic;
		adS :	std_logic_vector(4 downto 0);
		IMED :	std_logic_vector(31 downto 0);
		RA :	std_logic_vector(31 downto 0);
		RB :	std_logic_vector(31 downto 0);
		RALU :	std_logic_vector(31 downto 0);
		salta : 	std_logic;
		MDR : std_logic_vector(31 downto 0);
		adD : std_logic_vector(4 downto 0);
	 end record;
         
end p_MRstd;


--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Generic register  
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;

entity regnbit is
           generic( INIT_VALUE : STD_LOGIC_VECTOR(31 downto 0) := (others=>'0') );
           port(  ck, rst, ce : in std_logic;
                  D : in  STD_LOGIC_VECTOR (31 downto 0);
                  Q : out STD_LOGIC_VECTOR (31 downto 0)
               );
end regnbit;

architecture regn of regnbit is 
begin

  process(ck, rst)
  begin
       if rst = '1' then
              Q <= INIT_VALUE(31 downto 0);
       elsif ck'event and ck = '0' then
           if ce = '1' then
              Q <= D; 
           end if;
       end if;
  end process;
        
end regn;

library IEEE;
use IEEE.std_logic_1164.all;

entity regnbit_pc is
           generic( INIT_VALUE : STD_LOGIC_VECTOR(31 downto 0) := (others=>'0') );
           port(  ck, rst, ce : in std_logic;
                  D : in  STD_LOGIC_VECTOR (31 downto 0);
                  Q : out STD_LOGIC_VECTOR (31 downto 0)
               );
end regnbit_pc;

architecture regn_pc of regnbit_pc is 
begin

  process(ck, rst)
  begin
       if rst = '1' then
              Q <= INIT_VALUE(31 downto 0);
       elsif ck'event and ck = '1' then
           if ce = '1' then
              Q <= D; 
           end if;
       end if;
  end process;
        
end regn_pc;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Register Bank (R0..R31) - 31 GENERAL PURPOSE 16-bit REGISTERS
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.Std_Logic_1164.all;
use ieee.STD_LOGIC_UNSIGNED.all;   
use work.p_MRstd.all;

entity reg_bank is
       port( ck, rst, wreg :    in std_logic;
             AdRs, AdRt, adRd : in std_logic_vector( 4 downto 0);
             RD : in std_logic_vector(31 downto 0);
             R1, R2: out std_logic_vector(31 downto 0) 
           );
end reg_bank;

architecture reg_bank of reg_bank is
   type bank is array(0 to 31) of std_logic_vector(31 downto 0);
   signal reg : bank ;                            
   signal wen : std_logic_vector(31 downto 0) ;
begin            

    g1: for i in 0 to 31 generate        

        -- Remember register $0 is the constant 0, not a register.
        -- This is implemented by never enabling writes to register $0
        wen(i) <= '1' when i/=0 and adRD=i and wreg='1' else '0';
         
        -- Remember register $29, the stack pointer, points to some place
        -- near the bottom of the data memory, not the usual place 
		-- assigned by the MIPS simulator!!
        g2: if i=29 generate -- SP ---  x10010000 + x800 -- top of stack
           r29: entity work.regnbit generic map(INIT_VALUE=>x"10010800")    
                                  port map(ck=>ck, rst=>rst, ce=>wen(i), D=>RD, Q=>reg(i));
        end generate;  
                
        g3: if i/=29 generate 
           rx: entity work.regnbit port map(ck=>ck, rst=>rst, ce=>wen(i), D=>RD, Q=>reg(i));                    
        end generate;
                   
   end generate g1;   
    

    R1 <= reg(CONV_INTEGER(AdRs));    -- source1 selection 

    R2 <= reg(CONV_INTEGER(AdRt));    -- source2 selection 
   
end reg_bank;



--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- ALU - operation depends only on the current instruction 
--       (decoded in the control unit)
--
-- 22/11/2004 - subtle error correctionwas done for J!
-- Part of the work for J has been done before, by shifting IR(15 downto 0)
-- left by two bits before writing data to the IMED register 
--
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.std_logic_arith.all;
use work.p_MRstd.all;

entity alu is
       port( op1, op2 : in std_logic_vector(31 downto 0);
             outalu :   out std_logic_vector(31 downto 0);   
             op_alu : in inst_type   
           );
end alu;

architecture alu of alu is 
   signal menorU, menorS : std_logic ;
begin
  
    menorU <=  '1' when op1 < op2 else '0';
    menorS <=  '1' when ieee.Std_Logic_signed."<"(op1,  op2) else '0' ; -- signed
    
    outalu <=  
        op1 - op2                                when  op_alu=SUBU                     else
        op1 and op2                              when  op_alu=AAND  or op_alu=ANDI     else 
        op1 or  op2                              when  op_alu=OOR   or op_alu=ORI      else 
        op1 xor op2                              when  op_alu=XXOR  or op_alu=XORI     else 
        op1 nor op2                              when  op_alu=NNOR                     else 
        op2(15 downto 0) & x"0000"               when  op_alu=LUI                      else
        (0=>menorU, others=>'0')                 when  op_alu=SLTU  or op_alu=SLTIU    else   -- signed
        (0=>menorS, others=>'0')                 when  op_alu=SLT   or op_alu=SLTI     else   -- unsigned
        op1(31 downto 28) & op2(27 downto 0)     when  op_alu=J     or op_alu=JAL      else 
        op1                                      when  op_alu=JR    or op_alu=JALR     else 
        to_StdLogicVector(to_bitvector(op1) sll  CONV_INTEGER(op2(10 downto 6)))   when  op_alu=SSLL   else 
        to_StdLogicVector(to_bitvector(op2) sll  CONV_INTEGER(op1(5 downto 0)))    when  op_alu=SLLV   else 
        to_StdLogicVector(to_bitvector(op1) sra  CONV_INTEGER(op2(10 downto 6)))   when  op_alu=SSRA   else 
        to_StdLogicVector(to_bitvector(op2) sra  CONV_INTEGER(op1(5 downto 0)))    when  op_alu=SRAV   else 
        to_StdLogicVector(to_bitvector(op1) srl  CONV_INTEGER(op2(10 downto 6)))   when  op_alu=SSRL   else 
        to_StdLogicVector(to_bitvector(op2) srl  CONV_INTEGER(op1(5 downto 0)))    when  op_alu=SRLV   else 
        op1 + op2;    -- default for ADDU,ADDIU,LBU,LW,SW,SB,BEQ,BGEZ,BLEZ,BNE    

end alu;

library IEEE;
use IEEE.Std_Logic_1164.all;
use work.p_MRstd.all;

entity BI_DI_BAR is
		  generic( INIT_VALUE : STD_LOGIC_VECTOR(31 downto 0) := (others=>'0'));
        port(   ck, rst, ce : in std_logic;          
                BI : in BI_CONT;
					 DI : out DI_CONT
             );
end BI_DI_BAR;

architecture BI_DI_BAR of BI_DI_BAR is

begin

process(ck, rst)
  begin
       if rst = '1' then
		 
			DI.ir <=	INIT_VALUE(31 downto 0);
			DI.npc <= INIT_VALUE(31 downto 0);
			DI.i <=	NOP;
			DI.wreg <= INIT_VALUE(0);
			DI.rw <= INIT_VALUE(0);
			DI.bw	<= INIT_VALUE(0);
			DI.cem <= INIT_VALUE(0);
			DI.inst_branch <=	INIT_VALUE(0);          
			DI.inst_grupo1 <=	INIT_VALUE(0);
			DI.inst_grupoI <=	INIT_VALUE(0);
			
       elsif ck'event and ck = '1' then	--
           if ce = '1' then
			  
				DI.ir <=	BI.ir;
				DI.npc <= BI.npc;
				DI.i <= BI.i;	
				DI.wreg <= BI.wreg ;
				DI.rw <= BI.rw; 
				DI.bw	<= BI.bw; 
				DI.cem <= BI.cem; 
				DI.inst_branch <=	BI.inst_branch;         
				DI.inst_grupo1 <= BI.inst_grupo1;	
				DI.inst_grupoI <=	BI.inst_grupoI;
				
           end if;
       end if;
  end process;

end BI_DI_BAR;

library IEEE;
use IEEE.Std_Logic_1164.all;
use work.p_MRstd.all;

entity DI_EX_BAR is
		  generic( INIT_VALUE : STD_LOGIC_VECTOR(31 downto 0) := (others=>'0'));
        port(   ck, rst, ce : in std_logic;          
                DI : in DI_CONT;
					 R1, R2, cte_im : in STD_LOGIC_VECTOR(31 downto 0);
					 adS : in STD_LOGIC_VECTOR(4 downto 0);
					 EX : out EX_CONT
             );
end DI_EX_BAR;

architecture DI_EX_BAR of DI_EX_BAR is

begin

process(ck, rst)
  begin
       if rst = '1' then
		 
			EX.ir <=	INIT_VALUE;
			EX.npc <= INIT_VALUE;
			EX.i <=	NOP;
			EX.wreg <= INIT_VALUE(0);
			EX.rw <= INIT_VALUE(0);
			EX.bw	<= INIT_VALUE(0);
			EX.cem <= INIT_VALUE(0);
			EX.inst_branch <=	INIT_VALUE(0);          
			EX.inst_grupo1 <=	INIT_VALUE(0);
			EX.inst_grupoI <=	INIT_VALUE(0);
			EX.adS <= INIT_VALUE(4 downto 0);
			EX.IMED <= INIT_VALUE;
			EX.RA <= INIT_VALUE;
			EX.RB <= INIT_VALUE;
			
       elsif ck'event and ck = '1' then	--
		 --Forwarding--congela o que est no estado
		--	if (ce = '1' and ((WB.ir(26 downto 20)=EX.RA)or ME.ir(26 downto 20)=EX.RA))then
		--		EX.RA=EX.RA;
		--	elsif(ce = '1' and ((WB.ir(26 downto 20)=EX.RB)or ME.ir(26 downto 20)=EX.RB))then
		--			EX.RA=EX.RA;
					--	elsif ce = '1' then
						if ce = '1' then
							EX.ir <=	DI.ir;
							EX.npc <= DI.npc;
							EX.i <= DI.i;	
							EX.wreg <= DI.wreg ;
							EX.rw <= DI.rw; 
							EX.bw	<= DI.bw; 
							EX.cem <= DI.cem; 
							EX.inst_branch <=	DI.inst_branch;         
							EX.inst_grupo1 <= DI.inst_grupo1;	
							EX.inst_grupoI <=	DI.inst_grupoI;
							EX.adS <= adS;
							EX.IMED <= cte_im;
							EX.RA <= R1;
							EX.RB <= R2;
					--	end if;
		--		end if;
           end if;
       end if;
  end process;

end DI_EX_BAR;

library IEEE;
use IEEE.Std_Logic_1164.all;
use work.p_MRstd.all;

entity EX_ME_BAR is
		  generic( INIT_VALUE : STD_LOGIC_VECTOR(31 downto 0) := (others=>'0'));
        port(   ck, rst, ce : in std_logic;          
                EX : in EX_CONT;
					 outalu, RB_2, RA_2 : in STD_LOGIC_VECTOR(31 downto 0);
					 adD : in STD_LOGIC_VECTOR(4 downto 0);
					 salta : in std_logic;
					 ME : out ME_CONT
             );
end EX_ME_BAR;

architecture EX_ME_BAR of EX_ME_BAR is

begin

process(ck, rst)
  begin
       if rst = '1' then
		 
			ME.ir <=	INIT_VALUE;
			ME.npc <= INIT_VALUE;
			ME.i <=	NOP;
			ME.wreg <= INIT_VALUE(0);
			ME.rw <= INIT_VALUE(0);
			ME.bw	<= INIT_VALUE(0);
			ME.cem <= INIT_VALUE(0);
			ME.inst_branch <=	INIT_VALUE(0);          
			ME.inst_grupo1 <=	INIT_VALUE(0);
			ME.inst_grupoI <=	INIT_VALUE(0);
			ME.adS <= INIT_VALUE(4 downto 0);
			ME.IMED <= INIT_VALUE;
			ME.RA <= INIT_VALUE;
			ME.RB <= INIT_VALUE;
			ME.RALU <= INIT_VALUE;
			ME.salta <=	INIT_VALUE(0);
			ME.adD <= INIT_VALUE(4 downto 0);
       elsif ck'event and ck = '1' then	--
           if ce = '1' then
			  
				ME.ir <=	EX.ir;
				ME.npc <= EX.npc;
				ME.i <= EX.i;	
				ME.wreg <= EX.wreg ;
				ME.rw <= EX.rw; 
				ME.bw	<= EX.bw; 
				ME.cem <= EX.cem; 
				ME.inst_branch <=	EX.inst_branch;         
				ME.inst_grupo1 <= EX.inst_grupo1;	
				ME.inst_grupoI <=	EX.inst_grupoI;
				ME.adS <= EX.adS;
				ME.IMED <= EX.IMED;
				ME.RA <= RA_2;
				ME.RB <= RB_2;
				ME.RALU <= outalu when ja_saltou = '0'else
					   salva_endereco;
				ME.salta <=	salta;
				ME.adD <= adD;
           end if;
       end if;
  end process;

end EX_ME_BAR;

library IEEE;
use IEEE.Std_Logic_1164.all;
use work.p_MRstd.all;

entity ME_WB_BAR is
		  generic( INIT_VALUE : STD_LOGIC_VECTOR(31 downto 0) := (others=>'0'));
        port(   ck, rst, ce : in std_logic;          
                ME : in ME_CONT;
					 mdr_int : STD_LOGIC_VECTOR(31 downto 0);
					 WB : out WB_CONT
             );
end ME_WB_BAR;

architecture ME_WB_BAR of ME_WB_BAR is

begin

process(ck, rst)
  begin
       if rst = '1' then
		 
			WB.ir <=	INIT_VALUE;
			WB.npc <= INIT_VALUE;
			WB.i <=	NOP;
			WB.wreg <= INIT_VALUE(0);
			WB.rw <= INIT_VALUE(0);
			WB.bw	<= INIT_VALUE(0);
			WB.cem <= INIT_VALUE(0);
			WB.inst_branch <=	INIT_VALUE(0);          
			WB.inst_grupo1 <=	INIT_VALUE(0);
			WB.inst_grupoI <=	INIT_VALUE(0);
			WB.adS <= INIT_VALUE(4 downto 0);
			WB.IMED <= INIT_VALUE;
			WB.RA <= INIT_VALUE;
			WB.RB <= INIT_VALUE;
			WB.RALU <= INIT_VALUE;
			WB.salta <=	INIT_VALUE(0);
			WB.MDR <= INIT_VALUE;
			WB.adD <= INIT_VALUE(4 downto 0);
       elsif ck'event and ck = '1' then	--
           if ce = '1' then
			  
				WB.ir <=	ME.ir;
				WB.npc <= ME.npc;
				WB.i <= ME.i;	
				WB.wreg <= ME.wreg ;
				WB.rw <= ME.rw; 
				WB.bw	<= ME.bw; 
				WB.cem <= ME.cem; 
				WB.inst_branch <=	ME.inst_branch;         
				WB.inst_grupo1 <= ME.inst_grupo1;	
				WB.inst_grupoI <=	ME.inst_grupoI;
				WB.adS <= ME.adS;
				WB.IMED <= ME.IMED;
				WB.RA <= ME.RA;
				WB.RB <= ME.RB;
				WB.RALU <= ME.RALU;
				WB.salta <=	ME.salta;
				WB.MDR <= mdr_int;
				WB.adD <= ME.adD;
           end if;
       end if;
  end process;

end ME_WB_BAR;

library IEEE;
use IEEE.Std_Logic_1164.all;
use work.p_MRstd.all;

entity bubble_unit is
    port( clock, reset : in std_logic;
			i :  in inst_type;
			DI_RS, DI_RT, EX_RT : in std_logic_vector(4 downto 0);
			pause, condition :	out std_logic
			);
end bubble_unit;

architecture bubble_unit of bubble_unit is
	signal aux : std_logic;
begin
	aux <= '1' when (i=LW and ((EX_RT=DI_RS) or (EX_RT=DI_RT))) else '0';
	process(clock, reset)
		begin
			if reset = '1' then
				pause <= '0';
				condition <= '0';
			elsif clock'event and clock='0' then
				pause <= aux;
				condition <= '0';	
			elsif	clock'event and clock='1' and aux='1' then
				condition <= aux;
			end if;
    end process;
end bubble_unit;

library IEEE;
use IEEE.Std_Logic_1164.all;
use work.p_MRstd.all;

entity forward_unit is
    port(EX_inst_grupo1, WB_wreg, ME_wreg : in std_logic;
			ME_RD, WB_RD, EX_RT, EX_RS : in std_logic_vector(4 downto 0);
			forwardA, forwardB :	out std_logic_vector(1 downto 0)
			);
end forward_unit;

architecture forward_unit of forward_unit is
	signal auxA_WB, auxA_ME, auxB_WB, auxB_ME : std_logic;
begin
	auxA_ME <= '1' when ME_wreg='1' and ME_RD/="00000" and ME_RD=EX_RS else '0';
	auxA_WB <= '1' when WB_wreg='1' and WB_RD/="00000" and --ME_RD/=EX_RS and 
								WB_RD=EX_RS else '0';
	auxB_ME <= '1' when ME_wreg='1' and ME_RD/="00000" and ME_RD=EX_RT else '0';
	auxB_WB <= '1' when WB_wreg='1' and WB_RD/="00000" and --ME_RD/=EX_RT and 
								WB_RD=EX_RT else '0';

	forwardA <= "10" when auxA_ME='1' else
					"01" when auxA_WB='1' else "00";	
		
	forwardB <= "10" when auxB_ME='1' else 
					"01" when auxB_WB='1' else "00";
end forward_unit;

library IEEE;
use IEEE.Std_Logic_1164.all;
use work.p_MRstd.all;

entity dynamic_prediction_unit is
    port(EX_inst_branch, ja_saltou,salva_endereco,ck_in,rst_in : in std_logic;
			outalu:in std_logic_vector(31 downto 0);
			saida_dyn_pred: out std_logic(31 downto 0)
			);
end dynamic_prediction_unit;
--PT: Previsão Tomada
--PT_2: Previsão tomada_2, porém, anteriormente não foi tomada a previsão, chegando aqui.
--NT: Previsão Não Tomada
--NT: Previsão Não Tomada_2, porém, anteriormente foi tomada a previsão, chegando aqui.
architecture dynamic_prediction_unit of dynamic_prediction_unit is
	type predicted_state is(PT,PT_2,NT,NT2_);	
	signal st: predicted_state := PT;--st= state
	signal next_st; predicted_state ;= PT;
	signal conta_saltos<= std_logic;
begin
	fsm_moore_trasition:process(ck_in,rst_in)
	begin
		if(ck_in'event and ck_in ='1')then
			if(rst_in = '1')then
				st<= PT;
			else
				st<= next_st;
			end if;
		end if;
	end process fsm_moore_transition;

	conta_saltos<= ja_saltou;
	
	next_state:process(ck_in,rst_in,EX_inst_branch, ja_saltou)
	begin
		case st is
			when PT =>
				if(ck_in'event and ck_in'='1')then
					if(conta_saltos ='1' )then
						next_st <= PT;				
					else
						next_st<= PT_2;
					end if;
				end if;
			when PT_2 =>
				if(ck_in'event and ck_in'='1')then
					if(conta_saltos='1')then
						next_st<= PT;
					else
						next_st<= NT;
					end if;
			when NT =>
				if(ck_in'event and ck_in'='1')then
					if(conta_saltos='1')then
						next_st<=NT_2;
					else
						next_st<= NT;
					end if;				
			when NT_2 =>
				if(ck_in'event and ck_in'='1')then
					if(conta_saltos='1')then
						next_st<= PT;
					else
						next_st<= NT_2;
					end if;
	end process next_state;

	saida_dyn_pred<= salva_endereco when st= PT or st= PT_2 else
				outalu;

	 saida: entity work.datapath port map(saida_dyn_pred=saida_dyn_pred);
		
end dynamic_prediction_unit;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Datapath structural description
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.Std_Logic_1164.all;
use IEEE.Std_Logic_signed.all; -- needed for comparison instructions SLTxx
use IEEE.Std_Logic_arith.all; -- needed for comparison instructions SLTxx
use work.p_MRstd.all;
   
entity datapath is
      port(  ck, rst :     in std_logic;
	     saida_dyn_pred: in stdlogic(31 downto 0);
             i_address :   out std_logic_vector(31 downto 0);
             instruction : in std_logic_vector(31 downto 0);
             d_address :   out std_logic_vector(31 downto 0);
             data :        inout std_logic_vector(31 downto 0);  
             uins :        in microinstruction;
             IR_OUT :      out std_logic_vector(31 downto 0);
				 cem, rwm, bwm : out std_logic
          );
end datapath;

architecture datapath of datapath is
    signal incpc, pc, npc, IR,  result, R1, R2, RA, RB, RB_X, RA_X, RIN, ext16, cte_im, IMED, op1, op2, 
           outalu, RALU, MDR, mdr_int, dtpc, op1_R, op2_R, salva_endereco : std_logic_vector(31 downto 0) := (others=> '0');
    signal adD, adS : std_logic_vector(4 downto 0) := (others=> '0');    
    signal inst_branch, inst_grupo1, inst_grupoI, ja_saltou: std_logic;   
    signal salta, ce, cex, pause, condition, flush, jump, bubble, BI_DI_flush, 
			  DI_EX_flush, EX_ME_flush : std_logic := '0';
	 signal forwardA, forwardB : std_logic_vector(1 downto 0) := "00";		  
	 
	 signal BI : BI_CONT;
	 signal DI : DI_CONT;
	 signal EX : EX_CONT;
	 signal ME : ME_CONT;
	 signal WB : WB_CONT;
	 
begin
	ce <= '1';
	BI_DI_flush <= '1' when flush='1' else
					'0';
	DI_EX_flush<= '1' when bubble= '1' or flush='1' else
					'0';
	EX_ME_flush<= '1' when flush='1' else
					'0';
	 
	
   -- auxiliary signals 
   inst_branch  <= '1' when uins.i=BEQ or uins.i=BGEZ or uins.i=BLEZ or uins.i=BNE else 
                  '0';
                  
   inst_grupo1  <= '1' when uins.i=ADDU or uins.i=SUBU or uins.i=AAND
                         or uins.i=OOR or uins.i=XXOR or uins.i=NNOR else
                   '0';

   inst_grupoI  <= '1' when uins.i=ADDIU or uins.i=ANDI or uins.i=ORI or uins.i=XORI else
                   '0';

   --==============================================================================
   -- first_stage
   --==============================================================================
  
   incpc <= pc + 4;
  
   --RNPC: entity work.regnbit port map(ck=>ck, rst=>rst, ce=>uins.CY1, D=>incpc,       Q=>npc);     
           
   --RIR: entity work.regnbit  port map(ck=>ck, rst=>rst, ce=>uins.CY1, D=>instruction, Q=>IR);

   IR_OUT <= instruction;    -- IR is the datapath output signal to carry the instruction
             
   i_address <= pc;  -- connects PC output to the instruction memory address bus
	
		BI.ir <=	instruction;
		BI.npc <= incpc;	
		BI.i <=	uins.i;
		BI.wreg <= uins.wreg;
		BI.rw <= uins.rw;	
		BI.bw	<= uins.bw;
		BI.cem <= uins.cem;	
		BI.inst_branch <=	inst_branch;           
		BI.inst_grupo1 <=	inst_grupo1;
		BI.inst_grupoI <=	inst_grupoI;
   
   BI_DI_BAR: entity work.BI_DI_BAR port map(ck=>ck, rst=>BI_DI_flush, ce=>cex, BI=>BI, DI=>DI);
   --==============================================================================
   -- second stage
   --==============================================================================
                
   -- The then clause is only used for logic shifts with shamt field       
   adS <= DI.ir(20 downto 16) when DI.i=SSLL or DI.i=SSRA or DI.i=SSRL else 
          DI.ir(25 downto 21);
       
   REGS: entity work.reg_bank(reg_bank) port map
        (ck=>ck, rst=>rst, wreg=>WB.wreg, AdRs=>adS, AdRt=>DI.ir(20 downto 16), adRD=>Wb.adD,  
         Rd=>RIN, R1=>R1, R2=>R2);
    
   -- sign extension 
   ext16 <=  x"FFFF" & DI.ir(15 downto 0) when DI.ir(15)='1' else
             x"0000" & DI.ir(15 downto 0);
    
   -- Immediate constant
   cte_im <= ext16(29 downto 0)  & "00"     when DI.inst_branch='1'     else
                -- branch address adjustment for word frontier
             "0000" & DI.ir(25 downto 0) & "00" when DI.i=J or DI.i=JAL else
                -- J/JAL are word addressed. MSB four bits are defined at the ALU, not here!
             x"0000" & DI.ir(15 downto 0) when DI.i=ANDI or DI.i=ORI  or DI.i=XORI else
                -- logic instructions with immediate operand are zero extended
             ext16;
                -- The default case is used by addiu, lbu, lw, sbu and sw instructions

   -- second stage registers
   --REG_S:  entity work.regnbit port map(ck=>ck, rst=>rst, ce=>uins.CY2, D=>R1,     Q=>RA);

   --REG_T:  entity work.regnbit port map(ck=>ck, rst=>rst, ce=>uins.CY2, D=>R2,     Q=>RB);
  
   --REG_IM: entity work.regnbit port map(ck=>ck, rst=>rst, ce=>uins.CY2, D=>cte_im, Q=>IMED);
	DI_EX_BAR: entity work.DI_EX_BAR port map(ck=>ck, rst=>DI_EX_flush, ce=>cex, DI=>DI, EX=>EX,
																R1=>R1, R2=>R2, cte_im=>cte_im, adS=>adS);
 
  --==============================================================================
   -- third stage
   --==============================================================================
                      
   -- select the first ALU operand                           
   op1 <= EX.npc  when EX.inst_branch='1' else
			 RA_X; 
     
   -- select the second ALU operand
   op2 <= RB_X when EX.inst_grupo1='1' or EX.i=SLTU or EX.i=SLT or EX.i=JR 
                  or EX.i=SLLV or EX.i=SRAV or EX.i=SRLV else 
          EX.IMED;
			 	
	RA_X <= ME.RALU when forwardA="10" else 
			  result when forwardA="01" else EX.RA;
			  
	RB_X <= ME.RALU when forwardB="10" else 
			  result when forwardB="01" else EX.RB;
	
------------------------------------------------------------------------------------------------------
	salva_endereco <= outalu when EX.ins_banch = '1' and salta ='1' else
			 invalid_instruction ;
	ja_saltou<= '1' when  EX.ins_banch = '1' and salta ='1'else
		    '0';
-----------------------------------------------------------------------------------------------------
	dynamic_prediction_unit: entity work.dynamic_prediction_unit port map (ck_in=>ck, rst_in=>rst,EX_inst_branch=>EX.inst_brach, 				 											ja_saltou=>ja_saltou,salva_endereco=>salva_endereco);
-----------------------------------------------------------------------------------------------------
	cex <= ce and not(pause);

	forward_unit : entity work.forward_unit port map (EX_RT=>EX.ir(20 downto 16), 
									EX_RS=>EX.adS, ME_RD=>ME.adD, WB_RD=>WB.adD, WB_wreg=>WB.wreg , ME_wreg=>ME.wreg,
									forwardA=>forwardA, forwardB=>forwardB, EX_inst_grupo1=>EX.inst_grupo1);
				
	bubble_unit: entity work.bubble_unit port map (clock=>ck, reset=>rst, i=>EX.i, 
								EX_RT=>EX.ir(20 downto 16), DI_RT=>DI.ir(20 downto 16),
								DI_RS=>adS, pause=>pause, condition=>condition);
	
	bubble <= '1' when (pause='1') and (condition='1') else '0';
	-- register bank write address selection
   adD <= "11111"               when EX.i=JAL else -- JAL writes in register $31
         EX.ir(15 downto 11)       when EX.inst_grupo1='1' or EX.i=SLTU or EX.i=SLT
                                                     or EX.i=JALR  
						     or EX.i=SSLL or EX.i=SLLV
						     or EX.i=SSRA or EX.i=SRAV
						     or EX.i=SSRL or EX.i=SRLV
                                                     else
         EX.ir(20 downto 16) -- inst_grupoI='1' or uins.i=SLTIU or uins.i=SLTI 
        ;                 -- or uins.i=LW or  uins.i=LBU  or uins.i=LUI, or default		 
                 
   -- ALU instantiation
   inst_alu: entity work.alu port map (op1=>op1, op2=>op2, outalu=>saida_dyn_pred, op_alu=>EX.i);
                                   
   -- ALU register
   --REG_alu: entity work.regnbit  port map(ck=>ck, rst=>rst, ce=>uins.walu, D=>outalu, Q=>RALU);               
 
   -- evaluation of conditions to take the branch instructions
   salta <=  '1' when ( (RA_X=RB_X  and EX.i=BEQ)  or (RA_X>=0  and EX.i=BGEZ) or
                        (RA_X<=0  and EX.i=BLEZ) or (RA_X/=RB_X and EX.i=BNE) )  else
             '0';
	
                  
   EX_ME_BAR: entity work.EX_ME_BAR port map(ck=>ck, rst=>EX_ME_flush, ce=>ce, EX=>EX, ME=>ME,
															outalu=>outalu, salta=>salta, adD=>adD, RB_2=>RB_X, RA_2=>RA_X);          
   --==============================================================================
   -- fourth stage
   --==============================================================================
     
   d_address <= ME.RALU;
    
   -- tristate to control memory write    
   data <= ME.RB when (ME.cem='1' and ME.rw='0') else (others=>'Z');  
	cem <= ME.cem;
	rwm <= ME.rw;
	bwm <= ME.bw;
   -- single byte reading from memory  -- SUPONDO LITTLE ENDIAN
   mdr_int <= data when ME.i=LW  else
              x"000000" & data(7 downto 0);
       
   --RMDR: entity work.regnbit  port map(ck=>ck, rst=>rst, ce=>uins.wmdr, D=>mdr_int, Q=>MDR);                 
	ME_WB_BAR: entity work.ME_WB_BAR port map(ck=>ck, rst=>rst, ce=>'1', ME=>ME, WB=>WB,
															mdr_int=>mdr_int);
																												
   result <=    WB.MDR when WB.i=LW  or WB.i=LBU else
                WB.RALU;

   --==============================================================================
   -- fifth stage
   --==============================================================================

   -- signal to be written into the register bank
   RIN <= WB.npc when (WB.i=JALR or WB.i=JAL) else result;
    
   dtpc <= result when (WB.inst_branch='1' and WB.salta='1') or WB.i=J    or WB.i=JAL or WB.i=JALR or WB.i=JR  
	
           else incpc; --was else npc
	jump <= '1' when WB.i=J or WB.i=JAL or WB.i=JALR or WB.i=JR else '0'; 		  
   flush <= '1' when ((WB.inst_branch='1' and WB.salta='1') or jump='1') and (dtpc/=WB.npc) else
				'0';
   -- Code memory starting address: beware of the OFFSET! 
   -- The one below (x"00400000") serves for code generated 
   -- by the MARS simulator
   rpc: entity work.regnbit_pc generic map(INIT_VALUE=>x"00400000")   
                            port map(ck=>ck, rst=>rst, ce=>cex, D=>dtpc, Q=>pc);

end datapath;

--------------------------------------------------------------------------
--------------------------------------------------------------------------
--  Control Unit behavioral description 
--------------------------------------------------------------------------
--------------------------------------------------------------------------
library IEEE;
use IEEE.Std_Logic_1164.all;
use work.p_MRstd.all;

entity control_unit is
        port(   ck, rst : in std_logic;          
                uins : out microinstruction;
                ir : in std_logic_vector(31 downto 0)
             );
end control_unit;
                   
architecture control_unit of control_unit is
   type type_state is (Sidle, Sfetch, Sreg, Salu, Swbk, Sld, Sst, Ssalta);
   signal PS, NS : type_state;
   signal i : inst_type;      
begin
      
    ----------------------------------------------------------------------------------------
    -- BLOCK (1/3) - INSTRUCTION DECODING and ALU operation definition.
    -- This block generates 1 Output Function of the Control Unit
    ----------------------------------------------------------------------------------------
    i <=   ADDU   when ir(31 downto 26)="000000" and ir(10 downto 0)="00000100001" else
           SUBU   when ir(31 downto 26)="000000" and ir(10 downto 0)="00000100011" else
           AAND   when ir(31 downto 26)="000000" and ir(10 downto 0)="00000100100" else
           OOR    when ir(31 downto 26)="000000" and ir(10 downto 0)="00000100101" else
           XXOR   when ir(31 downto 26)="000000" and ir(10 downto 0)="00000100110" else
           NNOR   when ir(31 downto 26)="000000" and ir(10 downto 0)="00000100111" else
			  NOP		when ir(31 downto 0) = x"00000000" 										  else
           SSLL   when ir(31 downto 21)="00000000000" and ir(5 downto 0)="000000"  else
           SLLV   when ir(31 downto 26)="000000" and ir(10 downto 0)="00000000100" else
           SSRA   when ir(31 downto 21)="00000000000" and ir(5 downto 0)="000011"  else
           SRAV   when ir(31 downto 26)="000000" and ir(10 downto 0)="00000000111" else
           SSRL   when ir(31 downto 21)="00000000000" and ir(5 downto 0)="000010"  else
           SRLV   when ir(31 downto 26)="000000" and ir(10 downto 0)="00000000110" else
           ADDIU  when ir(31 downto 26)="001001" else
           ANDI   when ir(31 downto 26)="001100" else
           ORI    when ir(31 downto 26)="001101" else
           XORI   when ir(31 downto 26)="001110" else
           LUI    when ir(31 downto 26)="001111" else
           LW     when ir(31 downto 26)="100011" else
           LBU    when ir(31 downto 26)="100100" else
           SW     when ir(31 downto 26)="101011" else
           SB     when ir(31 downto 26)="101000" else
           SLTU   when ir(31 downto 26)="000000" and ir(5 downto 0)="101011" else
           SLT    when ir(31 downto 26)="000000" and ir(5 downto 0)="101010" else
           SLTIU  when ir(31 downto 26)="001011"                             else
           SLTI   when ir(31 downto 26)="001010"                             else
           BEQ    when ir(31 downto 26)="000100" else
           BGEZ   when ir(31 downto 26)="000001" and ir(20 downto 16)="00001" else
           BLEZ   when ir(31 downto 26)="000110" and ir(20 downto 16)="00000" else
           BNE    when ir(31 downto 26)="000101" else
           J      when ir(31 downto 26)="000010" else
           JAL    when ir(31 downto 26)="000011" else
           JALR   when ir(31 downto 26)="000000"  and ir(20 downto 16)="00000"
                                           and ir(10 downto 0) = "00000001001" else
           JR     when ir(31 downto 26)="000000" and ir(20 downto 0)="000000000000000001000" else
           invalid_instruction ; -- IMPORTANT: default condition is invalid instruction;
        
    assert i /= invalid_instruction
          report "******************* INVALID INSTRUCTION *************"
          severity error;
                   
    uins.i <= i;    -- this instructs the alu to execute its expected operation, if any

    ----------------------------------------------------------------------------------------
    -- BLOCK (2/3) - DATAPATH REGISTERS load control signals generation.
    ----------------------------------------------------------------------------------------
    --uins.CY1   <= '1' when PS=Sfetch         else '0';
            
    --uins.CY2   <= '1' when PS=Sreg           else '0';
  
    --uins.walu  <= '1' when PS=Salu           else '0';
                
    --uins.wmdr  <= '1' when PS=Sld            else '0';
  
    --uins.wreg   <= '1' when PS=Swbk or (PS=Ssalta and (i=JAL or i=JALR)) else   '0';
    uins.wreg   <= '0' when (i=SB or i=SW or i=J or i=JR or i=BEQ or i=BGEZ or i=BLEZ 
											or i=BNE) else '1';
	 
    uins.rw    <= '0' when (i=SB or i=SW) else  '1';
                  
    uins.cem    <= '1' when (i=SB or i=SW or i=LW or i=LBU) else '0';
    
    uins.bw    <= '0' when (i=SB) else '1';
      
    --uins.wpc   <= '1' when PS=Swbk or PS=Sst or PS=Ssalta  else  '0';
  
    ---------------------------------------------------------------------------------------------
    -- BLOCK (3/3) - Sequential part of the control unit - two processes implementing the
    -- Control Unit state register and the next-state (combinational) function
    --------------------------------------------------------------------------------------------- 
    --process(rst, ck)
    --begin
      --if rst='1' then
            --PS <= Sidle;          -- Sidle is the state the machine stays while processor is being reset
       --elsif ck'event and ck='1' then
       
            --if PS=Sidle then
                  --PS <= Sfetch;
            --else
                  --PS <= NS;
            --end if;
                
       --end if;
    --end process;
     
     
    --process(PS, i)
    --begin
       --case PS is         
      
            --when Sidle=>NS <= Sidle; -- reset being active, the processor do nothing!       

            -- first stage:  read the current instruction 
            --
            --when Sfetch=>NS <= Sreg;  
     
            -- second stage: read the register banck and store the mask (when i=stmsk)
            --
            --when Sreg=>NS <= Salu;  
             
            -- third stage: alu operation 
            --
            --when Salu =>if i=LBU  or i=LW then 
                                --NS <= Sld;  
                          --elsif i=SB or i=SW then 
                                --NS <= Sst;
                          --elsif i=J or i=JAL or i=JALR or i=JR or i=BEQ
                                    --or i=BGEZ or i=BLEZ  or i=BNE then 
                                --NS <= Ssalta;  
                          --else 
                                --NS <= Swbk; 
                          --end if;
                         
            -- fourth stage: data memory operation  
            --
            --when Sld=>  NS <= Swbk; 
            
            -- fifth clock cycle of most instructions  - GO BACK TO FETCH
            -- 
            --when Sst | Ssalta | Swbk=>NS <= Sfetch;
  
       --end case;

    --end process;
    
end control_unit;

--------------------------------------------------------------------------
-- Top-level instantiation of the MRstd Datapath and Control Unit
--------------------------------------------------------------------------
library IEEE;
use IEEE.Std_Logic_1164.all;
use work.p_MRstd.all;

entity MRstd is
    port( clock, reset: in std_logic;
          ce, rw, bw: out std_logic;
          i_address, d_address: out std_logic_vector(31 downto 0);
          instruction: in std_logic_vector(31 downto 0);
          data: inout std_logic_vector(31 downto 0));
end MRstd;

architecture MRstd of MRstd is
      signal IR: std_logic_vector(31 downto 0);
      signal uins: microinstruction;
 begin

     dp: entity work.datapath   
         port map( ck=>clock, rst=>reset, IR_OUT=>IR, uins=>uins, i_address=>i_address, 
                   instruction=>instruction, d_address=>d_address,  data=>data, cem=>ce,
						 rwm=>rw, bwm=>bw);

     ct: entity work.control_unit port map( ck=>clock, rst=>reset, IR=>IR, uins=>uins);
         
     --ce <= uins.cem;
     --rw <= uins.rw; 
     --bw <= uins.bw;
     
end MRstd;
