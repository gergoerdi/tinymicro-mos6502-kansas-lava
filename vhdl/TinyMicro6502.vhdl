library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use work.lava.all;
use work.all;

entity TinyMicro6502 is
  port(CLK_32MHZ : in std_logic;
       RESET : in std_logic;
       VGA_VSYNC : out std_logic;
       VGA_HSYNC : out std_logic;
       VGA_R : out std_logic_vector(3 downto 0);
       VGA_G : out std_logic_vector(3 downto 0);
       VGA_B : out std_logic_vector(3 downto 0));
end entity TinyMicro6502;
architecture str of TinyMicro6502 is
  signal CLK_40MHZ : std_logic;
  signal CLK_1MHZ : std_logic;
  
  signal MAIN_VRAM_WE : std_logic;
  signal MAIN_VRAM_W_ADDR : std_logic_vector(9 downto 0);
  signal MAIN_VRAM_W_DATA : std_logic_vector(3 downto 0);
  
  signal VIDEO_VRAM_ADDR : std_logic_vector(9 downto 0);
  signal VIDEO_VRAM_DATA : std_logic_vector(3 downto 0);
  
  signal CLKDIV : std_logic_vector(5 downto 0) := (others => '0');
  signal CLK_8KHZ : std_logic := '1';
 
begin
  inst_clockman : entity work.clockman
  port map (CLK_IN1 => CLK_32MHZ,
            RESET => RESET,
            CLK_OUT1 => CLK_40MHZ,
            CLK_OUT2 => CLK_1MHZ);
            
 proc_CLK_8KHZ: process(CLK_1MHZ,RESET) is
 begin
   if RESET = '1' then
     CLKDIV <= (others => '0');
     CLK_8KHZ <= '1';
   elsif rising_edge(CLK_1MHZ) then
    CLKDIV <= std_logic_vector(unsigned(CLKDIV) + 1);
    if CLKDIV = "000000" then
      CLK_8KHZ <= not CLK_8KHZ;
    end if;
   end if;
 end process proc_CLK_8KHZ;
            
  inst_MainBoard : entity work.MainBoard
  port map (CLK_1MHZ => CLK_8KHZ,
            RESET => RESET,
            VIDEO_WE => MAIN_VRAM_WE,
            VIDEO_W_ADDR => MAIN_VRAM_W_ADDR,
            VIDEO_W_DATA => MAIN_VRAM_W_DATA);
            
  inst_vram : entity work.bram_tdp
  port map (a_clk => CLK_8KHZ,
            a_wr => MAIN_VRAM_WE,
            a_addr => MAIN_VRAM_W_ADDR,
            a_din => MAIN_VRAM_W_DATA,
            a_dout => open,
            
            b_clk => CLK_40MHZ,
            b_wr => '0',
            b_addr => VIDEO_VRAM_ADDR,
            b_din => (others => '0'),
            b_dout => VIDEO_VRAM_DATA);
            
  inst_video : entity work.Video
  port map (CLK_40MHZ => CLK_40MHZ,
            RESET => RESET,
            VIDEO_R_DATA => VIDEO_VRAM_DATA,
            VIDEO_R_ADDR => VIDEO_VRAM_ADDR,
            VGA_VSYNC => VGA_VSYNC,
            VGA_HSYNC => VGA_HSYNC,
            VGA_R => VGA_R,
            VGA_G => VGA_G,
            VGA_B => VGA_B);
end architecture str;
