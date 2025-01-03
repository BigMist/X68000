LIBRARY	IEEE;
USE	IEEE.STD_LOGIC_1164.ALL;
USE	IEEE.STD_LOGIC_UNSIGNED.ALL;

entity heademu is
generic(
	maxtrack	:integer	:=79;
	maxset		:integer	:=10;
	initseek	:integer	:=0
);
port(
	desttrack	:in integer range 0 to maxtrack;
	destset		:in std_logic;
	setwait		:in integer range 0 to maxset;		--settling time
	
	curtrack	:out integer range 0 to maxtrack;
	
	reachtrack	:out std_logic;
	busy		:out std_logic;
	
	track0n		:out std_logic;
	
	init		:in std_logic;
	seekerr		:out std_logic;
	
	sft			:in std_logic;
	clk			:in std_logic;
	rstn		:in std_logic
);
end heademu;

architecture rtl of heademu is
signal	current	:integer range 0 to maxtrack;

	type state_t is (
		ST_INIIN0,
		ST_INIIN1,
		ST_INIOUT,
		ST_IDLE,
		ST_CONTROL,
		ST_WAITSET,
		ST_NOP
	);
	signal	state	:state_t;
	
	type movestate_t is(
		MS_IDLE,
		MS_SETDIR,
		MS_DIRWAIT,
		MS_SEEKON,
		MS_SEEKOFF,
		MS_SEEKWAIT
	);
	signal	movestate	:movestate_t;
	

signal	movedir		:std_logic;
signal	move		:std_logic;
signal	intbusy		:std_logic;
signal	curdir		:std_logic;
signal	wsetcount	:integer range 0 to maxset;
signal	seekerrb	:std_logic;
signal	track0s		:std_logic;
begin
	
	track0s<='0' when current=0 else '1';
	track0n<=track0s;
	
	process(clk,rstn)begin
		if(rstn='0')then
			current<=0;
			movedir<='1';
			move<='0';
			reachtrack<='0';
			seekerrb<='0';
			if(initseek=0)then
				busy<='0';
				state<=ST_IDLE;
			else
				busy<='1';
				state<=ST_INIIN0;
			end if;
		elsif(clk' event and clk='1')then
			reachtrack<='0';
			move<='0';
			if(init='1')then
				STATE<=ST_INIIN0;
				current<=0;
				seekerrb<='0';
				busy<='1';
			end if;
			case state is
			when ST_INIIN0 =>
				if(intbusy='0')then
					if(track0s='0')then
						if(current<maxtrack)then
							movedir<='0';
							move<='1';
							current<=current+1;
						else
							seekerrb<='1';
							busy<='0';
							state<=ST_IDLE;
						end if;
					else
						current<=0;
						state<=st_INIIN1;
					end if;
				end if;
			when ST_INIIN1 =>
				if(intbusy='0')then
					if(current<1)then
						movedir<='0';
						move<='1';
						current<=current+1;
					else
						current<=maxtrack;
						state<=st_INIOUT;
					end if;
				end if;
			when ST_INIOUT =>
				if(intbusy='0')then
					if(track0s='1')then
						if(current>0)then
							current<=current-1;
							movedir<='1';
							move<='1';
						else
							seekerrb<='1';
							busy<='0';
							state<=ST_IDLE;
						end if;
					else
						current<=0;
						seekerrb<='0';
						wsetcount<=setwait;
						state<=ST_WAITSET;
					end if;
				end if;
			when ST_IDLE =>
				if(destset='1')then
					if(current=desttrack)then
						reachtrack<='1';
						state<=ST_NOP;
						busy<='1';
					else
						if(desttrack>current)then
							movedir<='0';
						else
							movedir<='1';
						end if;
						state<=ST_CONTROL;
						busy<='1';
					end if;
				end if;
			when ST_NOP =>
				busy<='0';
				state<=st_IDLE;
			when ST_CONTROL =>
				if(intbusy='0')then
					if(current=desttrack)then
						wsetcount<=setwait;
						state<=ST_WAITSET;
					else
						move<='1';
						if(movedir='1')then
							current<=current-1;
						else
							current<=current+1;
						end if;
					end if;
				end if;
			when ST_WAITSET =>
				if(sft='1')then
					if(wsetcount>0)then
						wsetcount<=wsetcount-1;
					else
						reachtrack<='1';
						busy<='0';
						state<=ST_IDLE;
					end if;
				end if;
			when others=>
				state<=ST_IDLE;
			end case;
		end if;
	end process;
	
	process(clk,rstn)begin
		if(rstn='0')then
			curdir<='0';
			movestate<=MS_IDLE;
		elsif(clk' event and clk='1')then
			if(move='1')then
				if(curdir=movedir)then
					movestate<=MS_SEEKON;
				else
					movestate<=MS_SETDIR;
				end if;
			elsif(sft='1')then
				case movestate is
				when MS_SETDIR =>
					curdir<=movedir;
					movestate<=MS_DIRWAIT;
				when MS_DIRWAIT =>
					movestate<=MS_SEEKON;
				when MS_SEEKON =>
					movestate<=MS_SEEKOFF;
				when MS_SEEKOFF =>
					movestate<=MS_SEEKWAIT;
				when MS_SEEKWAIT =>
					movestate<=MS_IDLE;
				when others =>
					movestate<=MS_IDLE;
				end case;
			end if;
		end if;
	end process;
	
	intbusy<=	'1' when move='1' else
				'1' when movestate/=MS_IDLE else
				'0';

	curtrack<=current;
	seekerr<=seekerrb and not (destset or init);


	
end rtl;
				
			
		
		


