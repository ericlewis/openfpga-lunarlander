//
// User core top-level
//
// Instantiated by the real top-level: apf_top
//

`default_nettype none

module core_top (

//
// physical connections
//

///////////////////////////////////////////////////
// clock inputs 74.25mhz. not phase aligned, so treat these domains as asynchronous

input   wire            clk_74a, // mainclk1
input   wire            clk_74b, // mainclk1 

///////////////////////////////////////////////////
// cartridge interface
// switches between 3.3v and 5v mechanically
// output enable for multibit translators controlled by pic32

// GBA AD[15:8]
inout   wire    [7:0]   cart_tran_bank2,
output  wire            cart_tran_bank2_dir,

// GBA AD[7:0]
inout   wire    [7:0]   cart_tran_bank3,
output  wire            cart_tran_bank3_dir,

// GBA A[23:16]
inout   wire    [7:0]   cart_tran_bank1,
output  wire            cart_tran_bank1_dir,

// GBA [7] PHI#
// GBA [6] WR#
// GBA [5] RD#
// GBA [4] CS1#/CS#
//     [3:0] unwired
inout   wire    [7:4]   cart_tran_bank0,
output  wire            cart_tran_bank0_dir,

// GBA CS2#/RES#
inout   wire            cart_tran_pin30,
output  wire            cart_tran_pin30_dir,
// when GBC cart is inserted, this signal when low or weak will pull GBC /RES low with a special circuit
// the goal is that when unconfigured, the FPGA weak pullups won't interfere.
// thus, if GBC cart is inserted, FPGA must drive this high in order to let the level translators
// and general IO drive this pin.
output  wire            cart_pin30_pwroff_reset,

// GBA IRQ/DRQ
inout   wire            cart_tran_pin31,
output  wire            cart_tran_pin31_dir,

// infrared
input   wire            port_ir_rx,
output  wire            port_ir_tx,
output  wire            port_ir_rx_disable, 

// GBA link port
inout   wire            port_tran_si,
output  wire            port_tran_si_dir,
inout   wire            port_tran_so,
output  wire            port_tran_so_dir,
inout   wire            port_tran_sck,
output  wire            port_tran_sck_dir,
inout   wire            port_tran_sd,
output  wire            port_tran_sd_dir,
 
///////////////////////////////////////////////////
// cellular psram 0 and 1, two chips (64mbit x2 dual die per chip)

output  wire    [21:16] cram0_a,
inout   wire    [15:0]  cram0_dq,
input   wire            cram0_wait,
output  wire            cram0_clk,
output  wire            cram0_adv_n,
output  wire            cram0_cre,
output  wire            cram0_ce0_n,
output  wire            cram0_ce1_n,
output  wire            cram0_oe_n,
output  wire            cram0_we_n,
output  wire            cram0_ub_n,
output  wire            cram0_lb_n,

output  wire    [21:16] cram1_a,
inout   wire    [15:0]  cram1_dq,
input   wire            cram1_wait,
output  wire            cram1_clk,
output  wire            cram1_adv_n,
output  wire            cram1_cre,
output  wire            cram1_ce0_n,
output  wire            cram1_ce1_n,
output  wire            cram1_oe_n,
output  wire            cram1_we_n,
output  wire            cram1_ub_n,
output  wire            cram1_lb_n,

///////////////////////////////////////////////////
// sdram, 512mbit 16bit

output  wire    [12:0]  dram_a,
output  wire    [1:0]   dram_ba,
inout   wire    [15:0]  dram_dq,
output  wire    [1:0]   dram_dqm,
output  wire            dram_clk,
output  wire            dram_cke,
output  wire            dram_ras_n,
output  wire            dram_cas_n,
output  wire            dram_we_n,

///////////////////////////////////////////////////
// sram, 1mbit 16bit

output  wire    [16:0]  sram_a,
inout   wire    [15:0]  sram_dq,
output  wire            sram_oe_n,
output  wire            sram_we_n,
output  wire            sram_ub_n,
output  wire            sram_lb_n,

///////////////////////////////////////////////////
// vblank driven by dock for sync in a certain mode

input   wire            vblank,

///////////////////////////////////////////////////
// i/o to 6515D breakout usb uart

output  wire            dbg_tx,
input   wire            dbg_rx,

///////////////////////////////////////////////////
// i/o pads near jtag connector user can solder to

output  wire            user1,
input   wire            user2,

///////////////////////////////////////////////////
// RFU internal i2c bus 

inout   wire            aux_sda,
output  wire            aux_scl,

///////////////////////////////////////////////////
// RFU, do not use
output  wire            vpll_feed,


//
// logical connections
//

///////////////////////////////////////////////////
// video, audio output to scaler
output  wire    [23:0]  video_rgb,
output  wire            video_rgb_clock,
output  wire            video_rgb_clock_90,
output  wire            video_de,
output  wire            video_skip,
output  wire            video_vs,
output  wire            video_hs,
    
output  wire            audio_mclk,
input   wire            audio_adc,
output  wire            audio_dac,
output  wire            audio_lrck,

///////////////////////////////////////////////////
// bridge bus connection
// synchronous to clk_74a
output  wire            bridge_endian_little,
input   wire    [31:0]  bridge_addr,
input   wire            bridge_rd,
output  reg     [31:0]  bridge_rd_data,
input   wire            bridge_wr,
input   wire    [31:0]  bridge_wr_data,

///////////////////////////////////////////////////
// controller data
// 
// key bitmap:
//   [0]    dpad_up
//   [1]    dpad_down
//   [2]    dpad_left
//   [3]    dpad_right
//   [4]    face_a
//   [5]    face_b
//   [6]    face_x
//   [7]    face_y
//   [8]    trig_l1
//   [9]    trig_r1
//   [10]   trig_l2
//   [11]   trig_r2
//   [12]   trig_l3
//   [13]   trig_r3
//   [14]   face_select
//   [15]   face_start
// joy values - unsigned
//   [ 7: 0] lstick_x
//   [15: 8] lstick_y
//   [23:16] rstick_x
//   [31:24] rstick_y
// trigger values - unsigned
//   [ 7: 0] ltrig
//   [15: 8] rtrig
//
input   wire    [15:0]  cont1_key,
input   wire    [15:0]  cont2_key,
input   wire    [15:0]  cont3_key,
input   wire    [15:0]  cont4_key,
input   wire    [31:0]  cont1_joy,
input   wire    [31:0]  cont2_joy,
input   wire    [31:0]  cont3_joy,
input   wire    [31:0]  cont4_joy,
input   wire    [15:0]  cont1_trig,
input   wire    [15:0]  cont2_trig,
input   wire    [15:0]  cont3_trig,
input   wire    [15:0]  cont4_trig
    
);

// not using the IR port, so turn off both the LED, and
// disable the receive circuit to save power
assign port_ir_tx = 0;
assign port_ir_rx_disable = 1;

// bridge endianness
assign bridge_endian_little = 0;

// cart is unused, so set all level translators accordingly
// directions are 0:IN, 1:OUT
assign cart_tran_bank3 = 8'hzz;
assign cart_tran_bank3_dir = 1'b0;
assign cart_tran_bank2 = 8'hzz;
assign cart_tran_bank2_dir = 1'b0;
assign cart_tran_bank1 = 8'hzz;
assign cart_tran_bank1_dir = 1'b0;
assign cart_tran_bank0 = 4'hf;
assign cart_tran_bank0_dir = 1'b1;
assign cart_tran_pin30 = 1'b0;      // reset or cs2, we let the hw control it by itself
assign cart_tran_pin30_dir = 1'bz;
assign cart_pin30_pwroff_reset = 1'b0;  // hardware can control this
assign cart_tran_pin31 = 1'bz;      // input
assign cart_tran_pin31_dir = 1'b0;  // input

// link port is input only
assign port_tran_so = 1'bz;
assign port_tran_so_dir = 1'b0;     // SO is output only
assign port_tran_si = 1'bz;
assign port_tran_si_dir = 1'b0;     // SI is input only
assign port_tran_sck = 1'bz;
assign port_tran_sck_dir = 1'b0;    // clock direction can change
assign port_tran_sd = 1'bz;
assign port_tran_sd_dir = 1'b0;     // SD is input and not used

// tie off the rest of the pins we are not using
assign cram0_a = 'h0;
assign cram0_dq = {16{1'bZ}};
assign cram0_clk = 0;
assign cram0_adv_n = 1;
assign cram0_cre = 0;
assign cram0_ce0_n = 1;
assign cram0_ce1_n = 1;
assign cram0_oe_n = 1;
assign cram0_we_n = 1;
assign cram0_ub_n = 1;
assign cram0_lb_n = 1;

assign cram1_a = 'h0;
assign cram1_dq = {16{1'bZ}};
assign cram1_clk = 0;
assign cram1_adv_n = 1;
assign cram1_cre = 0;
assign cram1_ce0_n = 1;
assign cram1_ce1_n = 1;
assign cram1_oe_n = 1;
assign cram1_we_n = 1;
assign cram1_ub_n = 1;
assign cram1_lb_n = 1;

assign dram_a = 'h0;
assign dram_ba = 'h0;
assign dram_dq = {16{1'bZ}};
assign dram_dqm = 'h0;
assign dram_clk = 'h0;
assign dram_cke = 'h0;
assign dram_ras_n = 'h1;
assign dram_cas_n = 'h1;
assign dram_we_n = 'h1;

assign sram_a = 'h0;
assign sram_dq = {16{1'bZ}};
assign sram_oe_n  = 1;
assign sram_we_n  = 1;
assign sram_ub_n  = 1;
assign sram_lb_n  = 1;

assign dbg_tx = 1'bZ;
assign user1 = 1'bZ;
assign aux_scl = 1'bZ;
assign vpll_feed = 1'bZ;


// for bridge write data, we just broadcast it to all bus devices
// for bridge read data, we have to mux it
// add your own devices here
always @(*) begin
    casex(bridge_addr)
    default: begin
        bridge_rd_data <= 0;
    end
    32'h10xxxxxx: begin
        // example
        // bridge_rd_data <= example_device_data;
        bridge_rd_data <= 0;
    end
    32'hF8xxxxxx: begin
        bridge_rd_data <= cmd_bridge_rd_data;
    end
    endcase
end


//
// host/target command handler
//
    wire            reset_n;                // driven by host commands, can be used as core-wide reset
    wire    [31:0]  cmd_bridge_rd_data;
    
// bridge host commands
// synchronous to clk_74a
    wire            status_boot_done = pll_core_locked; 
    wire            status_setup_done = pll_core_locked; // rising edge triggers a target command
    wire            status_running = reset_n; // we are running as soon as reset_n goes high

    wire            dataslot_requestread;
    wire    [15:0]  dataslot_requestread_id;
    wire            dataslot_requestread_ack = 1;
    wire            dataslot_requestread_ok = 1;

    wire            dataslot_requestwrite;
    wire    [15:0]  dataslot_requestwrite_id;
    wire            dataslot_requestwrite_ack = 1;
    wire            dataslot_requestwrite_ok = 1;

    wire            dataslot_allcomplete;

    wire            savestate_supported;
    wire    [31:0]  savestate_addr;
    wire    [31:0]  savestate_size;
    wire    [31:0]  savestate_maxloadsize;

    wire            savestate_start;
    wire            savestate_start_ack;
    wire            savestate_start_busy;
    wire            savestate_start_ok;
    wire            savestate_start_err;

    wire            savestate_load;
    wire            savestate_load_ack;
    wire            savestate_load_busy;
    wire            savestate_load_ok;
    wire            savestate_load_err;
    
    wire            osnotify_inmenu;

// bridge target commands
// synchronous to clk_74a


// bridge data slot access

    wire    [9:0]   datatable_addr;
    wire            datatable_wren;
    wire    [31:0]  datatable_data;
    wire    [31:0]  datatable_q;

core_bridge_cmd icb (

    .clk                ( clk_74a ),
    .reset_n            ( reset_n ),

    .bridge_endian_little   ( bridge_endian_little ),
    .bridge_addr            ( bridge_addr ),
    .bridge_rd              ( bridge_rd ),
    .bridge_rd_data         ( cmd_bridge_rd_data ),
    .bridge_wr              ( bridge_wr ),
    .bridge_wr_data         ( bridge_wr_data ),
    
    .status_boot_done       ( status_boot_done ),
    .status_setup_done      ( status_setup_done ),
    .status_running         ( status_running ),

    .dataslot_requestread       ( dataslot_requestread ),
    .dataslot_requestread_id    ( dataslot_requestread_id ),
    .dataslot_requestread_ack   ( dataslot_requestread_ack ),
    .dataslot_requestread_ok    ( dataslot_requestread_ok ),

    .dataslot_requestwrite      ( dataslot_requestwrite ),
    .dataslot_requestwrite_id   ( dataslot_requestwrite_id ),
    .dataslot_requestwrite_ack  ( dataslot_requestwrite_ack ),
    .dataslot_requestwrite_ok   ( dataslot_requestwrite_ok ),

    .dataslot_allcomplete   ( dataslot_allcomplete ),

    .savestate_supported    ( savestate_supported ),
    .savestate_addr         ( savestate_addr ),
    .savestate_size         ( savestate_size ),
    .savestate_maxloadsize  ( savestate_maxloadsize ),

    .savestate_start        ( savestate_start ),
    .savestate_start_ack    ( savestate_start_ack ),
    .savestate_start_busy   ( savestate_start_busy ),
    .savestate_start_ok     ( savestate_start_ok ),
    .savestate_start_err    ( savestate_start_err ),

    .savestate_load         ( savestate_load ),
    .savestate_load_ack     ( savestate_load_ack ),
    .savestate_load_busy    ( savestate_load_busy ),
    .savestate_load_ok      ( savestate_load_ok ),
    .savestate_load_err     ( savestate_load_err ),

    .osnotify_inmenu        ( osnotify_inmenu ),
    
    .datatable_addr         ( datatable_addr ),
    .datatable_wren         ( datatable_wren ),
    .datatable_data         ( datatable_data ),
    .datatable_q            ( datatable_q ),

);



////////////////////////////////////////////////////////////////////////////////////////

wire clk_6, clk_25_175_90deg, clk_25, clk_50, clk_25_175;

wire    pll_core_locked;
    
mf_pllbase mp1 (
    .refclk         ( clk_74a ),
    .rst            ( 0 ),
    
    .outclk_0       ( clk_6 ),
    .outclk_1       ( clk_25_175_90deg ),
    .outclk_2       ( clk_50 ),
    .outclk_3       ( clk_25 ),
    .outclk_4       ( clk_25_175 ),

    .locked         ( pll_core_locked )
);


//////////////////////////////////////////////
// Core Data
//////////////////////////////////////////////

wire        ioctl_wr;
wire [24:0] ioctl_addr;
wire  [7:0] ioctl_dout;

data_loader_8 #(
    .ADDRESS_MASK_UPPER_4(0), 
    .WRITE_MEM_CLOCK_DELAY(1), 
    .WRITE_MEM_EN_CYCLE_LENGTH(1)
) rom_loader (
    .clk_74a(clk_74a),
    .clk_memory(clk_25),

    .reset_n(status_setup_done),

    .bridge_wr(bridge_wr),
    .bridge_endian_little(bridge_endian_little),
    .bridge_addr(bridge_addr),
    .bridge_wr_data(bridge_wr_data),

    .write_en(ioctl_wr),
    .write_addr(ioctl_addr),
    .write_data(ioctl_dout)
);

//////////////////////////////////////////////
// Core Audio
//////////////////////////////////////////////

  wire [7:0] audio;

  assign audio_mclk = audgen_mclk;
  assign audio_dac = audgen_dac;
  assign audio_lrck = audgen_lrck;

  reg				audgen_nextsamp;

  // generate MCLK = 12.288mhz with fractional accumulator
  reg         [21:0]  audgen_accum;
  reg                 audgen_mclk;
  parameter   [20:0]  CYCLE_48KHZ = 21'd122880 * 2;
  always @(posedge clk_74a)
  begin
    audgen_accum <= audgen_accum + CYCLE_48KHZ;
    if(audgen_accum >= 21'd742500)
    begin
      audgen_mclk <= ~audgen_mclk;
      audgen_accum <= audgen_accum - 21'd742500 + CYCLE_48KHZ;
    end
  end

  // generate SCLK = 3.072mhz by dividing MCLK by 4
  reg [1:0]   aud_mclk_divider;
  wire        audgen_sclk = aud_mclk_divider[1] /* synthesis keep*/;
  always @(posedge audgen_mclk)
  begin
    aud_mclk_divider <= aud_mclk_divider + 1'b1;
  end

  // shift out audio data as I2S
  // 32 total bits per channel, but only 16 active bits at the start and then 16 dummy bits
  //
  // synchronize audio samples coming from the core
  wire	[31:0]	audgen_sampdata_s;
  synch_3 #(.WIDTH(32)) s5(({1'b0, audio[7:1], 1'b0, audio[7:1]}), audgen_sampdata_s, audgen_sclk);
  reg		[31:0]	audgen_sampshift;
  reg		[4:0]	audgen_lrck_cnt;
  reg				audgen_lrck;
  reg				audgen_dac;
  always @(negedge audgen_sclk)
  begin
    // output the next bit
    audgen_dac <= audgen_sampshift[31];

    // 48khz * 64
    audgen_lrck_cnt <= audgen_lrck_cnt + 1'b1;
    if(audgen_lrck_cnt == 31)
    begin
      // switch channels
      audgen_lrck <= ~audgen_lrck;

      // Reload sample shifter
      if(~audgen_lrck)
      begin
        audgen_sampshift <= audgen_sampdata_s;
      end
    end
    else if(audgen_lrck_cnt < 16)
    begin
      // only shift for 16 clocks per channel
      audgen_sampshift <= {audgen_sampshift[30:0], 1'b0};
    end
  end

//////////////////////////////////////////////
// Core Video
//////////////////////////////////////////////

wire hblank, vblank_lunarlander;
wire ohblank, ovblank;
wire vgade;

wire hs, vs;
wire ohs, ovs, ode;
wire [2:0] r,g,b;
wire [7:0] outr,outg,outb;

reg [3:0] r2;
reg [3:0] g2;
reg [3:0] b2;

always @(posedge clk_50) begin
  r2 <= outr[7:5];
	g2 <= outg[7:5];
  b2 <= outb[7:5];
end

ovo #(.COLS(1), .LINES(1), .RGB(24'hFF00FF)) diff (
	.i_r({r,r,r[2:1]}),
	.i_g({g,g,g[2:1]}),
	.i_b({b,b,b[2:1]}),
	.i_hs(~hs),
	.i_vs(~vs),
	.i_de(vgade),
	.i_hblank(hblank),
	.i_vblank(vblank_lunarlander),
	.i_en(clk_25),
	.i_clk(clk_50),

	.o_r(outr),
	.o_g(outg),
	.o_b(outb),
	.o_hs(ohs),
	.o_vs(ovs),
	.o_de(ode),
	.o_hblank(ohblank),
	.o_vblank(ovblank),

	.ena(diff_count > 0),

	.in0(difficulty),
	.in1(),
);

  assign video_rgb_clock = clk_25_175;
  assign video_rgb_clock_90 = clk_25_175_90deg;

  reg video_de_reg;
  reg video_hs_reg;
  reg video_vs_reg;
  reg [23:0] video_rgb_reg;
  reg video_skip_reg;

  assign video_de = video_de_reg;
  assign video_hs = video_hs_reg;
  assign video_vs = video_vs_reg;
  assign video_rgb = video_rgb_reg;
  assign video_skip = video_skip_reg;

  reg hs_prev;
  reg vs_prev;
  reg de_prev;

  always @(posedge clk_25_175)
  begin
    video_de_reg <= 0;
    video_rgb_reg <= 24'h0;

    if (~(ovblank || ohblank)) begin
      video_de_reg <= 1;

      video_rgb_reg[23:16] <= {2{r2}};
      video_rgb_reg[15:8]  <= {2{g2}};
      video_rgb_reg[7:0]   <= {2{b2}};
    end

    video_hs_reg <= ~hs_prev && ohs;
    video_vs_reg <= ~vs_prev && ovs;
    hs_prev <= ohs;
    vs_prev <= ovs;
  end

//////////////////////////////////////////////
// Core Controls
//////////////////////////////////////////////

wire [15:0] cont1_key_s;
wire [15:0] cont2_key_s;

synch_2 #(
  .WIDTH(16)
) cont1_s (
  cont1_key,
  cont1_key_s,
  clk_6
);

synch_2 #(
  .WIDTH(16)
) cont2_s (
  cont2_key,
  cont2_key_s,
  clk_6
);

reg [7:0] dpad_thrust = 0;

synch_2 #(
  .WIDTH(8)
) thrust_s (
  dpad_thrust,
  in_thrust,
  clk_6
);

// 1 second = 50,000,000 cycles (duh)
// If we want to go from zero to full throttle in 1 second we tick every
// 196,850 cycles.
always @(posedge clk_50) begin :thrust_count
	int thrust_count;
	thrust_count <= thrust_count + 1'd1;

	if (thrust_count == 'd196_850) begin
		thrust_count <= 0;
		if ((cont1_key_s[5] | cont2_key_s[5]) && dpad_thrust > 0)
			dpad_thrust <= dpad_thrust - 1'd1;

		if ((cont1_key_s[4] | cont2_key_s[4]) && dpad_thrust < 'd254)
			dpad_thrust <= dpad_thrust + 1'd1;
	end
end

//4     5      6    7     8          9
//Start,Select,Coin,Abort,Turn Right,Turn Left

wire in_select = ~(cont1_key_s[14] | cont2_key_s[14]);
wire in_start  = ~(cont1_key_s[15] | cont2_key_s[15]);
wire in_turn_l = ~(cont1_key_s[2]  | cont2_key_s[2]);
wire in_turn_r = ~(cont1_key_s[3]  | cont2_key_s[3]);
wire in_coin   = ~(cont1_key_s[9]  | cont2_key_s[9]);
wire in_abort  = ~(cont1_key_s[8]  | cont2_key_s[8]);

wire [7:0] in_thrust;

//////////////////////////////////////////////
// Core Instance
//////////////////////////////////////////////

wire lamp2, lamp3, lamp4, lamp5;

wire [1:0] difficulty;

always_comb begin
	if(lamp5)
		difficulty = 2'd3;
	else if(lamp4)
		difficulty = 2'd2;
	else if(lamp3)
		difficulty = 2'd1;
	else
		difficulty = 2'd0;
end

int diff_count = 0;
always @(posedge clk_50) begin
	if (diff_count > 0)
		diff_count <= diff_count - 1;
	
	if (~in_select)
		diff_count <= 'd500_000_000; // 10 seconds
end

wire is_starting;
wire [7:0] m_dip = {1'b0,1'b0,1'b1,1'b1,1'b0,1'b1,1'b1,1'b1};

LLANDER_TOP LLANDER_TOP
(
	.ROT_LEFT_L(in_turn_l),
	.ROT_RIGHT_L(in_turn_r),
	.ABORT_L(in_abort),
	.GAME_SEL_L(in_select),
	.START_L(in_start),
	.COIN1_L(in_coin),
	.COIN2_L(in_coin),
	.THRUST(in_thrust),
	.DIAG_STEP_L(1'b1),
	.SLAM_L(1'b1),
	.SELF_TEST_L(1'b1), 
	.START_SEL_L(is_starting),
	.LAMP2(lamp2),
	.LAMP3(lamp3),
	.LAMP4(lamp4),
	.LAMP5(lamp5),

	.AUDIO_OUT(audio),
	.dn_addr(ioctl_addr[15:0]),
	.dn_data(ioctl_dout),
	.dn_wr(ioctl_wr),	
	.VIDEO_R_OUT(r),
	.VIDEO_G_OUT(g),
	.VIDEO_B_OUT(b),
	.HSYNC_OUT(hs),
	.VSYNC_OUT(vs),
	.VGA_DE(vgade),
	.VID_HBLANK(hblank),
	.VID_VBLANK(vblank_lunarlander),
	.DIP(m_dip),
	.RESET_L (reset_n),	
	.clk_6(clk_6),
	.clk_25(clk_25)
);
    
endmodule
