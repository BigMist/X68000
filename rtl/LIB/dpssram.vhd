LIBRARY ieee;
USE ieee.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity dpssram is
generic(
	awidth	:integer	:=8;
	dwidth	:integer	:=8
);
port(
	addr1	:in std_logic_vector(awidth-1 downto 0);
	wdat1	:in std_logic_vector(dwidth-1 downto 0);
	wr1	:in std_logic;
	rdat1	:out std_logic_vector(dwidth-1 downto 0);
	
	addr2	:in std_logic_vector(awidth-1 downto 0);
	wdat2	:in std_logic_vector(dwidth-1 downto 0);
	wr2	:in std_logic;
	rdat2	:out std_logic_vector(dwidth-1 downto 0);
	
	clk	:in std_logic
);
end dpssram;

architecture rtl of dpssram is
subtype DAT_LAT_TYPE is std_logic_vector(dwidth-1 downto 0); 
type DAT_LAT_ARRAY is array (natural range <>) of DAT_LAT_TYPE; 
constant arange	:integer	:=2**awidth;
signal	DAT_LAT	:DAT_LAT_ARRAY(0 to arange-1);
signal	iaddr1,iaddr2	:integer range 0 to arange-1;

begin
	iaddr1<=conv_integer(addr1);
	iaddr2<=conv_integer(addr2);
	
	process(clk)begin
		if(clk' event and clk='1')then
			if(wr1='1')then
				DAT_LAT(iaddr1)<=wdat1;
			end if;
			if(wr2='1')then
				DAT_LAT(iaddr2)<=wdat2;
			end if;
			rdat1<=DAT_LAT(iaddr1);
			rdat2<=DAT_LAT(iaddr2);
		end if;
	end process;
end rtl;

