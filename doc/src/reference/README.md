# Assembler reference

This section strives to be a developer reference for the SPC700 instruction set. It is based on a couple of other documents: mainly a [very comprehensive SNES APU manual](https://web.archive.org/web/20060208001231/http://www.alpha-ii.com/snesmusic/files/spc700_apu_manual.txt), [Gau's higher-level overview](http://emureview.ztnet.com/developerscorner/SoundCPU/spc.htm), the [nocash SNES hardware specification](https://problemkaputt.de/fullsnes.htm#snesapudspbrrsamples), and the [Asar manual](https://rpghacker.github.io/asar/manual/). Note that because spcasm mostly concerns itself with SPC700 programming, I/O aspects are barely documented here; the nocash specification and the APU manual have very detailed information on how the DSP works and how it can be utilized.

**This section is incomplete.**
- [ ] Document all mnemonics spcasm recognizes and what they do.
- [ ] Document all addressing modes and what they do.
- [ ] Document expression syntax.
- [ ] Document the label system and its limitations.
- [ ] Document all macros and what they do.
