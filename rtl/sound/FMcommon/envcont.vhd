LIBRARY	IEEE,work;
	USE	IEEE.STD_LOGIC_1164.ALL;
	USE	IEEE.STD_LOGIC_UNSIGNED.ALL;
	use ieee.std_logic_arith.all;
	use work.envelope_pkg.all;

entity envcont is
generic(
	totalwidth	:integer	:=28
);
port(
	KEY		:in std_logic;
	AR		:in std_logic_vector(4 downto 0);
	DR		:in std_logic_vector(4 downto 0);
	SLlevel	:in std_logic_vector(15 downto 0);
	RR		:in std_logic_vector(3 downto 0);
	SR		:in std_logic_vector(4 downto 0);
	RKS		:in std_logic_vector(4 downto 0);
	
	CURSTATE	:in envstate_t;
	NXTSTATE	:out envstate_t;
	
	CURLEVEL	:in std_logic_vector(totalwidth-1 downto 0);
	NXTLEVEL	:out std_logic_vector(totalwidth-1 downto 0)
);

end envcont;

architecture rtl of envcont is
signal	SLEVEL	:std_logic_vector(totalwidth downto 0);
signal	RATE	:std_logic_vector(5 downto 0);
signal	RSEL	:std_logic_vector(6 downto 0);
signal	DELTA	:std_logic_vector(totalwidth-1 downto 0);
begin
	
	SLEVEL(totalwidth downto totalwidth-16)<='0' & SLlevel;
	SLEVEL(totalwidth-17 downto 0)<=(others=>'1');
	
	RSEL<=	('0' & AR & '0') +"0010000"	when CURSTATE=es_Atk else
				'0' & DR & '0'	when CURSTATE=es_Dec else
				'0' & SR & '0'	when CURSTATE=es_Sus else
				'0' & RR & "00";
	
	process(RSEL,RKS)
	variable tmp	:std_logic_vector(6 downto 0);
	begin
		tmp:=RSEL+("00" & RKS);
		if(tmp(6)='1')then
			tmp(6):='0';
			tmp(5 downto 0):=(others=>'1');
		end if;
		RATE<=tmp(5 downto 0);
	end process;
	
	process(RATE)
	variable ibit	:integer range 0 to 15;
	variable tmp	:std_logic_vector(totalwidth-1 downto 0);
	begin
		ibit:=conv_integer(RATE(5 downto 2));
		tmp:=(others=>'0');
		case RATE(1 downto 0) is
		when "00" =>
			tmp(ibit+7 downto ibit):="10000000";	--2^7
		when "01" =>
			tmp(ibit+7 downto ibit):="10011000";	--2^7.25
		when "10" =>
			tmp(ibit+7 downto ibit):="10110101";	--2^7.5
		when "11" =>
			tmp(ibit+7 downto ibit):="11010111";	--2^7.75
		when others =>
			tmp(ibit+7 downto ibit):="10000000";
		end case;
		DELTA<=tmp;
	end process;

	process(KEY,AR,DR,SLEVEL,RR,SR,CURSTATE,CURLEVEL)
	variable venvlev	:std_logic_vector(totalwidth downto 0);
	variable adddat		:std_logic_vector(totalwidth downto 0);
	begin
		NXTSTATE<=CURSTATE;
		if(KEY='1')then
			case CURSTATE is
			when es_OFF | es_Rel =>
				NXTSTATE<=es_Atk;
				NXTLEVEL<=(others=>'0');
			when es_Atk =>
				if(AR="00000")then
					adddat:=(others=>'0');
				elsif(RATE="111111")then
					adddat(totalwidth):='0';
					adddat(totalwidth-1 downto 0):=(others=>'1');
				else
					adddat:=(others=>'0');
					case CURLEVEL(totalwidth-1 downto totalwidth-3)is
					when "000" | "001" | "010" | "011" =>
						adddat:='0' & DELTA;
						if(AR(0)='1')then
							adddat:=adddat+DELTA(totalwidth-1 downto 1);
						end if;
					when "100" | "101" =>
						adddat:="00" & DELTA(totalwidth-1 downto 1);
						if(AR(0)='1')then
							adddat:=adddat+DELTA(totalwidth-1 downto 2);
						end if;
					when "110" =>
						adddat:="000" & DELTA(totalwidth-1 downto 2);
						if(AR(0)='1')then
							adddat:=adddat+DELTA(totalwidth-1 downto 3);
						end if;
					when others =>
						adddat:="0000" & DELTA(totalwidth-1 downto 3);
						if(AR(0)='1')then
							adddat:=adddat+DELTA(totalwidth-1 downto 4);
						end if;
					end case;
				end if;
				venvlev:=('0' & CURLEVEL) + adddat;
				if(venvlev(totalwidth)='1')then
					NXTLEVEL<=(others=>'1');
					NXTSTATE<=es_Dec;
				else
					NXTLEVEL<=venvlev(totalwidth-1 downto 0);
				end if;
			when es_Dec =>
				adddat:=(others=>'0');
				if(DR/="00000")then
					adddat:='0' & DELTA;
				end if;
				venvlev:=('0' & CURLEVEL)-adddat;
				if(venvlev(totalwidth)='1' or venvlev<SLEVEL)then
					NXTLEVEL<=SLEVEL(totalwidth-1 downto 0);
					NXTSTATE<=es_Sus;
				else
					NXTLEVEL<=venvlev(totalwidth-1 downto 0);
				end if;
			when es_Sus =>
				adddat:=(others=>'0');
				if(SR/="00000")then
					adddat:='0' & DELTA;
				end if;
				venvlev:=('0' & CURLEVEL) - adddat;
				if(venvlev(totalwidth)='1')then
					NXTLEVEL<=(others=>'0');
				else
					NXTLEVEL<=venvlev(totalwidth-1 downto 0);
				end if;
			when others =>
				NXTLEVEL<=(others=>'0');
				NXTSTATE<=es_OFF;
			end case;
		else
			case CURSTATE is
			when es_OFF =>
				NXTLEVEL<=(others=>'0');
			when others =>
				adddat:=(others=>'0');
				adddat:='0' & DELTA;
				venvlev:=('0' & CURLEVEL) - adddat;
				if(venvlev(totalwidth)='1')then
					NXTLEVEL<=(others=>'0');
					NXTSTATE<=es_OFF;
				else
					NXTLEVEL<=venvlev(totalwidth-1 downto 0);
					NXTSTATE<=es_Rel;
				end if;
			end case;
		end if;
	end process;
	
end rtl;
		
	