LIBRARY	IEEE;
	USE	IEEE.STD_LOGIC_1164.ALL;
	USE	IEEE.STD_LOGIC_ARITH.ALL;
	USE	IEEE.STD_LOGIC_UNSIGNED.ALL;

entity datfifo is
generic(
	depth		:integer	:=32;
	dwidth	:integer	:=8
);
port(
	datin		:in std_logic_vector(dwidth-1 downto 0);
	datwr		:in std_logic;
	
	datout	:out std_logic_vector(dwidth-1 downto 0);
	datrd		:in std_logic;
	
	indat		:out std_logic;
	buffull	:out std_logic;
	datnum	:out integer range 0 to depth-1;
	
	clr		:in std_logic	:='0';
	
	clk		:in std_logic;
	rstn		:in std_logic
);
end datfifo;

architecture rtl of datfifo is
signal	wraddr	:integer range 0 to depth-1;
signal	rdaddr	:integer range 0 to depth-1;
subtype DAT_LAT_TYPE is std_logic_vector(dwidth-1 downto 0); 
type DAT_LAT_ARRAY is array (natural range <>) of DAT_LAT_TYPE; 
signal	RAM	:DAT_LAT_ARRAY(0 to depth-1);

begin
	process(clk,rstn)begin
		if(rstn='0')then
			wraddr<=0;
		elsif(clk' event and clk='1')then
			if(clr='1')then
				wraddr<=0;
			elsif(datwr='1')then
				RAM(wraddr)<=datin;
				wraddr<=wraddr+1;
			end if;
		end if;
	end process;
	
	datout<=RAM(rdaddr);
	
	process(clk,rstn)begin
		if(rstn='0')then
			rdaddr<=0;
		elsif(clk' event and clk='1')then
			if(clr='1')then
				rdaddr<=0;
			elsif(datrd='1')then
				rdaddr<=rdaddr+1;
			end if;
		end if;
	end process;
	
	indat<=	'0' when wraddr=rdaddr else '1';
	buffull<='1' when wraddr+1=rdaddr else '0';
	datnum<=wraddr-rdaddr;
end rtl;

