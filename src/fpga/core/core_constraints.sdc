#
# user core constraints
#
# put your clock groups in here as well as any net assignments
#

create_clock -name {clk_74a} -period 13.468 -waveform { 0.000 6.734 } [get_ports {clk_74a}]
create_clock -name {clk_74b} -period 13.468 -waveform { 0.000 6.734 } [get_ports {clk_74b}]
create_clock -name {bridge_spiclk} -period 13.468 -waveform { 0.000 6.734 } [get_ports {bridge_spiclk}]
create_clock -name {core_top:ic|aud_mclk_divider[1]} -period 10.000 -waveform { 0.000 5.000 } 
create_clock -name {core_top:ic|audgen_mclk} -period 10.000 -waveform { 0.000 5.000 } 

set_clock_groups -asynchronous \
 -group { bridge_spiclk } \
 -group { clk_74a } \
 -group { clk_74b } \
 -group { \ 
    ic|mp1|mf_pllbase_inst|altera_pll_i|general[0].gpll~PLL_OUTPUT_COUNTER|divclk \
 } \
 -group { \
    ic|mp1|mf_pllbase_inst|altera_pll_i|general[1].gpll~PLL_OUTPUT_COUNTER|divclk \
 } \
 -group { \
    ic|mp1|mf_pllbase_inst|altera_pll_i|general[2].gpll~PLL_OUTPUT_COUNTER|divclk \
 } \
 -group { \
    ic|mp1|mf_pllbase_inst|altera_pll_i|general[3].gpll~PLL_OUTPUT_COUNTER|divclk \
 } \
-group { \
    ic|mp1|mf_pllbase_inst|altera_pll_i|general[4].gpll~PLL_OUTPUT_COUNTER|divclk \
 }

derive_pll_clocks
derive_clock_uncertainty