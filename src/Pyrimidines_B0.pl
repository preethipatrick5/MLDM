%:- multifile modeh/2.
%:- multifile modeb/2.
%:- retractall.
%:- retractall(great(A)).

%Module Loading Starts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(aleph).
:- if(current_predicate(use_rendering/1)).
:- use_rendering(prolog).
:- endif.
:- aleph.
:-style_check(-discontiguous).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Module Loading ends

:- aleph_set(i,2).
%:- aleph_set(depth,2).
%:- aleph_set(noise,5).
%:- aleph_set(clauselength,4).

:- modeh(1,great(+drug,+drug)).

:- modeb(*,drug(+drug)).
:- modeb(*,struc(+drug,-element,-element,-element)).
:- modeb(*,polar(+element,-polarval)).
:- modeb(*,polar(+element,#polarval)).
:- modeb(*,size(+element,-sizeval)).
:- modeb(*,size(+element,#sizeval)).
:- modeb(*,flex(+element,-flexval)).
:- modeb(*,flex(+element,#flexval)).
:- modeb(*,h_doner(+element,-donerval)).
:- modeb(*,h_doner(+element,#donerval)).
:- modeb(*,h_acceptor(+element,-acceptorval)).
:- modeb(*,h_acceptor(+component,#acceptorval)).
:- modeb(*,pi_doner(+element,-pi_donerval)).
:- modeb(*,pi_doner(+element,#pi_donnerval)).
:- modeb(*,pi_acceptor(+element,-pi_acceptorval)).
:- modeb(*,pi_acceptor(+element,#pi_acceptorval)).
:- modeb(*,polarisable(+element,-polarisableval)).
:- modeb(*,polarisable(+element,#polarisableval)).
:- modeb(*,sigma(+element,-sigmaval)).
:- modeb(*,sigma(+element,#sigmaval)).

:- modeb(1,gt(+polarval,+polarval)).
:- modeb(1,gt(+sizeval,+sizeval)).
:- modeb(1,gt(+flexval,+flexval)).
:- modeb(1,gt(+donerval,+donerval)).
:- modeb(1,gt(+acceptorval,+acceptorval)).
:- modeb(1,gt(+pi_donerval,+pi_donerval)).
:- modeb(1,gt(+pi_acceptorval,+pi_acceptorval)).
:- modeb(1,gt(+polarisableval,+polarisableval)).
:- modeb(1,gt(+sigmaval,+sigmaval)).

:- determination(great/2,gt/2).
:- determination(great/2,flex/2).
:- determination(great/2,flex0/1).
:- determination(great/2,flex0x/1).
:- determination(great/2,flex1/1).
:- determination(great/2,flex1x/1).
:- determination(great/2,flex2/1).
:- determination(great/2,flex2x/1).
:- determination(great/2,flex3/1).
:- determination(great/2,flex3x/1).
:- determination(great/2,flex4/1).
:- determination(great/2,flex4x/1).
:- determination(great/2,flex6/1).
:- determination(great/2,flex6x/1).
:- determination(great/2,flex7/1).
:- determination(great/2,flex7x/1).
:- determination(great/2,flex8/1).
:- determination(great/2,flex8x/1).
:- determination(great/2,great0_flex/1).
:- determination(great/2,great0_h_acc/1).
:- determination(great/2,great0_h_don/1).
:- determination(great/2,great0_pi_acc/1).
:- determination(great/2,great0_pi_don/1).
:- determination(great/2,great0_polar/1).
:- determination(great/2,great0_polari/1).
:- determination(great/2,great0_sigma/1).
:- determination(great/2,great0_size/1).
:- determination(great/2,great1_flex/1).
:- determination(great/2,great1_h_acc/1).
:- determination(great/2,great1_h_don/1).
:- determination(great/2,great1_pi_acc/1).
:- determination(great/2,great1_pi_don/1).
:- determination(great/2,great1_polar/1).
:- determination(great/2,great1_polari/1).
:- determination(great/2,great1_sigma/1).
:- determination(great/2,great1_size/1).
:- determination(great/2,great2_flex/1).
:- determination(great/2,great2_h_acc/1).
:- determination(great/2,great2_h_don/1).
:- determination(great/2,great2_pi_acc/1).
:- determination(great/2,great2_pi_don/1).
:- determination(great/2,great2_polar/1).
:- determination(great/2,great2_polari/1).
:- determination(great/2,great2_sigma/1).
:- determination(great/2,great2_size/1).
:- determination(great/2,great3_flex/1).
:- determination(great/2,great3_h_acc/1).
:- determination(great/2,great3_h_don/1).
:- determination(great/2,great3_pi_acc/1).
:- determination(great/2,great3_pi_don/1).
:- determination(great/2,great3_polar/1).
:- determination(great/2,great3_polari/1).
:- determination(great/2,great3_sigma/1).
:- determination(great/2,great3_size/1).
:- determination(great/2,great4_flex/1).
:- determination(great/2,great4_h_acc/1).
:- determination(great/2,great4_h_don/1).
:- determination(great/2,great4_pi_acc/1).
:- determination(great/2,great4_pi_don/1).
:- determination(great/2,great4_polar/1).
:- determination(great/2,great4_polari/1).
:- determination(great/2,great4_sigma/1).
:- determination(great/2,great4_size/1).
:- determination(great/2,great5_flex/1).
:- determination(great/2,great5_h_acc/1).
:- determination(great/2,great5_h_don/1).
:- determination(great/2,great5_pi_acc/1).
:- determination(great/2,great5_pi_don/1).
:- determination(great/2,great5_polar/1).
:- determination(great/2,great5_polari/1).
:- determination(great/2,great5_sigma/1).
:- determination(great/2,great5_size/1).
:- determination(great/2,great6_flex/1).
:- determination(great/2,great6_h_acc/1).
:- determination(great/2,great6_h_don/1).
:- determination(great/2,great6_pi_acc/1).
:- determination(great/2,great6_pi_don/1).
:- determination(great/2,great6_polar/1).
:- determination(great/2,great6_polari/1).
:- determination(great/2,great6_sigma/1).
:- determination(great/2,great6_size/1).
:- determination(great/2,great7_flex/1).
:- determination(great/2,great7_h_acc/1).
:- determination(great/2,great7_h_don/1).
:- determination(great/2,great7_pi_acc/1).
:- determination(great/2,great7_pi_don/1).
:- determination(great/2,great7_polar/1).
:- determination(great/2,great7_polari/1).
:- determination(great/2,great7_sigma/1).
:- determination(great/2,great7_size/1).
:- determination(great/2,great_flex/2).
:- determination(great/2,great_h_acc/2).
:- determination(great/2,great_h_don/2).
:- determination(great/2,great_pi_acc/2).
:- determination(great/2,great_pi_don/2).
:- determination(great/2,great_polar/2).
:- determination(great/2,great_polari/2).
:- determination(great/2,great_sigma/2).
:- determination(great/2,great_size/2).
:- determination(great/2,h_acceptor/2).
:- determination(great/2,h_acceptor0/1).
:- determination(great/2,h_acceptor0x/1).
:- determination(great/2,h_acceptor1/1).
:- determination(great/2,h_acceptor1x/1).
:- determination(great/2,h_acceptor2/1).
:- determination(great/2,h_acceptor2x/1).
:- determination(great/2,h_doner/2).
:- determination(great/2,h_doner0/1).
:- determination(great/2,h_doner0x/1).
:- determination(great/2,h_doner1/1).
:- determination(great/2,h_doner1x/1).
:- determination(great/2,h_doner2/1).
:- determination(great/2,h_doner2x/1).
:- determination(great/2,less2_flex/1).
:- determination(great/2,less2_h_acc/1).
:- determination(great/2,less2_h_don/1).
:- determination(great/2,less2_pi_acc/1).
:- determination(great/2,less2_pi_don/1).
:- determination(great/2,less2_polar/1).
:- determination(great/2,less2_polari/1).
:- determination(great/2,less2_sigma/1).
:- determination(great/2,less2_size/1).
:- determination(great/2,less3_flex/1).
:- determination(great/2,less3_h_acc/1).
:- determination(great/2,less3_h_don/1).
:- determination(great/2,less3_pi_acc/1).
:- determination(great/2,less3_pi_don/1).
:- determination(great/2,less3_polar/1).
:- determination(great/2,less3_polari/1).
:- determination(great/2,less3_sigma/1).
:- determination(great/2,less3_size/1).
:- determination(great/2,less4_flex/1).
:- determination(great/2,less4_h_acc/1).
:- determination(great/2,less4_h_don/1).
:- determination(great/2,less4_pi_acc/1).
:- determination(great/2,less4_pi_don/1).
:- determination(great/2,less4_polar/1).
:- determination(great/2,less4_polari/1).
:- determination(great/2,less4_sigma/1).
:- determination(great/2,less4_size/1).
:- determination(great/2,less5_flex/1).
:- determination(great/2,less5_h_acc/1).
:- determination(great/2,less5_h_don/1).
:- determination(great/2,less5_pi_acc/1).
:- determination(great/2,less5_pi_don/1).
:- determination(great/2,less5_polar/1).
:- determination(great/2,less5_polari/1).
:- determination(great/2,less5_sigma/1).
:- determination(great/2,less5_size/1).
:- determination(great/2,less6_flex/1).
:- determination(great/2,less6_h_acc/1).
:- determination(great/2,less6_h_don/1).
:- determination(great/2,less6_pi_acc/1).
:- determination(great/2,less6_pi_don/1).
:- determination(great/2,less6_polar/1).
:- determination(great/2,less6_polari/1).
:- determination(great/2,less6_sigma/1).
:- determination(great/2,less6_size/1).
:- determination(great/2,less7_flex/1).
:- determination(great/2,less7_h_acc/1).
:- determination(great/2,less7_h_don/1).
:- determination(great/2,less7_pi_acc/1).
:- determination(great/2,less7_pi_don/1).
:- determination(great/2,less7_polar/1).
:- determination(great/2,less7_polari/1).
:- determination(great/2,less7_sigma/1).
:- determination(great/2,less7_size/1).
:- determination(great/2,less8_flex/1).
:- determination(great/2,less8_h_acc/1).
:- determination(great/2,less8_h_don/1).
:- determination(great/2,less8_pi_acc/1).
:- determination(great/2,less8_pi_don/1).
:- determination(great/2,less8_polar/1).
:- determination(great/2,less8_polari/1).
:- determination(great/2,less8_sigma/1).
:- determination(great/2,less8_size/1).
:- determination(great/2,less9_flex/1).
:- determination(great/2,less9_h_acc/1).
:- determination(great/2,less9_h_don/1).
:- determination(great/2,less9_pi_acc/1).
:- determination(great/2,less9_pi_don/1).
:- determination(great/2,less9_polar/1).
:- determination(great/2,less9_polari/1).
:- determination(great/2,less9_sigma/1).
:- determination(great/2,less9_size/1).
:- determination(great/2,pi_acceptor/2).
:- determination(great/2,pi_acceptor0/1).
:- determination(great/2,pi_acceptor0x/1).
:- determination(great/2,pi_acceptor1/1).
:- determination(great/2,pi_acceptor1x/1).
:- determination(great/2,pi_acceptor2/1).
:- determination(great/2,pi_acceptor2x/1).
:- determination(great/2,pi_doner/2).
:- determination(great/2,pi_doner0/1).
:- determination(great/2,pi_doner0x/1).
:- determination(great/2,pi_doner1/1).
:- determination(great/2,pi_doner1x/1).
:- determination(great/2,pi_doner2/1).
:- determination(great/2,pi_doner2x/1).
:- determination(great/2,polar/2).
:- determination(great/2,polar0/1).
:- determination(great/2,polar0x/1).
:- determination(great/2,polar1/1).
:- determination(great/2,polar1x/1).
:- determination(great/2,polar2/1).
:- determination(great/2,polar2x/1).
:- determination(great/2,polar3/1).
:- determination(great/2,polar3x/1).
:- determination(great/2,polar4/1).
:- determination(great/2,polar4x/1).
:- determination(great/2,polar5/1).
:- determination(great/2,polar5x/1).
:- determination(great/2,polarisable/2).
:- determination(great/2,polarisable0/1).
:- determination(great/2,polarisable0x/1).
:- determination(great/2,polarisable1/1).
:- determination(great/2,polarisable1x/1).
:- determination(great/2,polarisable2/1).
:- determination(great/2,polarisable2x/1).
:- determination(great/2,polarisable3/1).
:- determination(great/2,polarisable3x/1).
:- determination(great/2,sigma/2).
:- determination(great/2,sigma0/1).
:- determination(great/2,sigma0x/1).
:- determination(great/2,sigma1/1).
:- determination(great/2,sigma1x/1).
:- determination(great/2,sigma2/1).
:- determination(great/2,sigma2x/1).
:- determination(great/2,sigma3/1).
:- determination(great/2,sigma3x/1).
:- determination(great/2,sigma5/1).
:- determination(great/2,sigma5x/1).
:- determination(great/2,size/2).
:- determination(great/2,size1/1).
:- determination(great/2,size1x/1).
:- determination(great/2,size2/1).
:- determination(great/2,size2x/1).
:- determination(great/2,size3/1).
:- determination(great/2,size3x/1).
:- determination(great/2,size4/1).
:- determination(great/2,size4x/1).
:- determination(great/2,size5/1).
:- determination(great/2,size5x/1).
:- determination(great/2,struc/4).


gt(X, Y) :-
	atom(X),
	atom(Y),
	name(X, XList),
	name(Y, YList),
	last(XList, XChar),
	last(YList, YChar),
	XVal is XChar - 48,
	YVal is YChar - 48,
	XVal > YVal.

:-begin_bg.

drug(d01).
drug(d02).
drug(d03).
drug(d04).
drug(d05).
drug(d06).
drug(d07).
drug(d08).
drug(d09).
drug(d10).
drug(d11).
drug(d12).
drug(d13).
drug(d14).
drug(d15).
drug(d16).
drug(d17).
drug(d18).
drug(d19).
drug(d20).
drug(d21).
drug(d22).
drug(d23).
drug(d24).
drug(d25).
drug(d26).
drug(d27).
drug(d28).
drug(d29).
drug(d30).
drug(d31).
drug(d32).
drug(d33).
drug(d34).
drug(d35).
drug(d36).
drug(d37).
drug(d38).
drug(d39).
drug(d40).
drug(d41).
drug(d42).
drug(d43).
drug(d44).
drug(d45).
drug(d46).
drug(d47).
drug(d48).
drug(d49).
drug(d50).
drug(d51).
drug(d52).
drug(d53).
drug(d54).
drug(d55).

element(oh).
element(xh).
element(h).
element(xobch2b6ch3).
element(xobch2b5ch3).
element(xno2).
element(f).
element(obch2b7ch3).
element(ch2oh).
element(xnh2).
element(xf).
element(obch2b6ch3).
element(xoch2ch2och3).
element(xcl).
element(xoh).
element(xch3).
element(och2ch2och3).
element(ch2obch2b3ch3).
element(och2conh2).
element(xocf3).
element(ch2och3).
element(cl).
element(ch3).
element(xnbch3b2).
element(xbr).
element(xoch3).
element(obch2b3ch3).
element(obch2b5ch3).
element(xobch2b3ch3).
element(xnhcoch3).
element(oso2ch3).
element(och3).
element(br).
element(no2).
element(och2c6h5).
element(cf3).
element(i).
element(xobch2b7ch3).
element(xoch2c6h5).
element(nbch3b2).
element(nhcoch3).
element(nh2).
element(ocf3).
element(xch2och3).
element(xch2obch2b3ch3).
element(xch2oh).
element(xoch2conh2).
element(xi).
element(xcf3).
element(xoso2ch3).

struc(d01,oh,xh,oh).
struc(d02,h,xobch2b6ch3,h).
struc(d03,h,xobch2b5ch3,h).
struc(d04,h,xh,h).
struc(d05,h,xno2,h).
struc(d06,f,xh,h).
struc(d07,obch2b7ch3,xh,h).
struc(d08,ch2oh,xh,h).
struc(d09,h,xnh2,h).
struc(d10,ch2oh,xh,ch2oh).
struc(d11,h,xf,h).
struc(d12,obch2b6ch3,xh,h).
struc(d13,h,xoch2ch2och3,h).
struc(d14,h,xcl,h).
struc(d15,oh,xoh,h).
struc(d16,oh,xh,h).
struc(d17,h,xch3,h).
struc(d18,och2ch2och3,xh,h).
struc(d19,ch2obch2b3ch3,xh,h).
struc(d20,och2conh2,xh,h).
struc(d21,h,xocf3,h).
struc(d22,ch2och3,xh,h).
struc(d23,cl,xh,h).
struc(d24,ch3,xh,h).
struc(d25,h,xnbch3b2,h).
struc(d26,h,xbr,h).
struc(d27,h,xoch3,h).
struc(d28,obch2b3ch3,xh,h).
struc(d29,obch2b5ch3,xh,h).
struc(d30,h,xobch2b3ch3,h).
struc(d31,h,xnhcoch3,h).
struc(d32,oso2ch3,xh,h).
struc(d33,och3,xh,h).
struc(d34,br,xh,h).
struc(d35,no2,xnhcoch3,h).
struc(d36,och2c6h5,xh,h).
struc(d37,cf3,xh,h).
struc(d38,och2ch2och3,xoch2ch2och3,h).
struc(d39,i,xh,h).
struc(d40,cf3,xoch3,h).
struc(d41,och3,xoch3,h).
struc(d42,och3,xoch2ch2och3,och3).
struc(d43,och3,xh,och3).
struc(d44,och3,xoch3,och3).
struc(d45,ch3,xoh,ch3).
struc(d46,ch3,xoch3,ch3).
struc(d47,och3,xobch2b5ch3,och3).
struc(d48,och3,xobch2b7ch3,och3).
struc(d49,och3,xoch2c6h5,och3).
struc(d50,och3,xch3,och3).
struc(d51,i,xoch3,i).
struc(d52,i,xoh,i).
struc(d53,br,xnh2,br).
struc(d54,cl,xnh2,cl).
struc(d55,cl,xnh2,ch3).

polar(ch3,polar0).
polar(ch2och3,polar0).
polar(ch2obch2b3ch3,polar0).
polar(nbch3b2,polar1).
polar(ch2oh,polar2).
polar(och3,polar2).
polar(och2ch2och3,polar2).
polar(obch2b3ch3,polar2).
polar(obch2b5ch3,polar2).
polar(obch2b6ch3,polar2).
polar(obch2b7ch3,polar2).
polar(och2c6h5,polar2).
polar(nhcoch3,polar3).
polar(och2conh2,polar3).
polar(nh2,polar3).
polar(i,polar3).
polar(br,polar3).
polar(cl,polar3).
polar(oh,polar3).
polar(cf3,polar3).
polar(ocf3,polar4).
polar(oso2ch3,polar4).
polar(f,polar5).
polar(no2,polar5).

polar(xch3,polar0).
polar(xch2och3,polar0).
polar(xch2obch2b3ch3,polar0).
polar(xnbch3b2,polar1).
polar(xch2oh,polar2).
polar(xoch3,polar2).
polar(xoch2ch2och3,polar2).
polar(xobch2b3ch3,polar2).
polar(xobch2b5ch3,polar2).
polar(xobch2b6ch3,polar2).
polar(xobch2b7ch3,polar2).
polar(xoch2c6h5,polar2).
polar(xnhcoch3,polar3).
polar(xoch2conh2,polar3).
polar(xnh2,polar3).
polar(xi,polar3).
polar(xbr,polar3).
polar(xcl,polar3).
polar(xoh,polar3).
polar(xcf3,polar3).
polar(xocf3,polar4).
polar(xoso2ch3,polar4).
polar(xf,polar5).
polar(xno2,polar5).


size(ch3,size1).
size(nh2,size1).
size(i,size1).
size(br,size1).
size(cl,size1).
size(oh,size1).
size(cf3,size1).
size(f,size1).
size(no2,size2).
size(nbch3b2,size2).
size(ch2och3,size2).
size(ch2oh,size2).
size(och3,size2).
size(nhcoch3,size2).
size(oso2ch3,size2).
size(ocf3,size3).
size(och2conh2,size3).
size(och2ch2och3,size3).
size(obch2b3ch3,size3).
size(ch2obch2b3ch3,size4).
size(och2c6h5,size4).
size(obch2b5ch3,size5).
size(obch2b6ch3,size5).
size(obch2b7ch3,size5).

size(xch3,size1).
size(xnh2,size1).
size(xi,size1).
size(xbr,size1).
size(xcl,size1).
size(xoh,size1).
size(xcf3,size1).
size(xf,size1).
size(xno2,size2).
size(xnbch3b2,size2).
size(xch2och3,size2).
size(xch2oh,size2).
size(xoch3,size2).
size(xnhcoch3,size2).
size(xoso2ch3,size2).
size(xocf3,size3).
size(xoch2conh2,size3).
size(xoch2ch2och3,size3).
size(xobch2b3ch3,size3).
size(xch2obch2b3ch3,size4).
size(xoch2c6h5,size4).
size(xobch2b5ch3,size5).
size(xobch2b6ch3,size5).
size(xobch2b7ch3,size5).


flex(f,flex0).
flex(oh,flex0).
flex(nh2,flex0).
flex(ch3,flex0).
flex(cl,flex0).
flex(no2,flex0).
flex(br,flex0).
flex(i,flex0).
flex(cf3,flex0).
flex(nbch3b2,flex0).
flex(nhcoch3,flex0).
flex(och3,flex1).
flex(ocf3,flex1).
flex(oso2ch3,flex1).
flex(ch2oh,flex2).
flex(och2c6h5,flex2).
flex(och2conh2,flex2).
flex(ch2och3,flex3).
flex(och2ch2och3,flex4).
flex(obch2b3ch3,flex4).
flex(ch2obch2b3ch3,flex6).
flex(obch2b5ch3,flex6).
flex(obch2b6ch3,flex7).
flex(obch2b7ch3,flex8).

flex(xf,flex0).
flex(xoh,flex0).
flex(xnh2,flex0).
flex(xch3,flex0).
flex(xcl,flex0).
flex(xno2,flex0).
flex(xbr,flex0).
flex(xi,flex0).
flex(xcf3,flex0).
flex(xnbch3b2,flex0).
flex(xnhcoch3,flex0).
flex(xoch3,flex1).
flex(xocf3,flex1).
flex(xoso2ch3,flex1).
flex(xch2oh,flex2).
flex(xoch2c6h5,flex2).
flex(xoch2conh2,flex2).
flex(xch2och3,flex3).
flex(xoch2ch2och3,flex4).
flex(xobch2b3ch3,flex4).
flex(xch2obch2b3ch3,flex6).
flex(xobch2b5ch3,flex6).
flex(xobch2b6ch3,flex7).
flex(xobch2b7ch3,flex8).

h_doner(f,h_don0).
h_doner(ch3,h_don0).
h_doner(cl,h_don0).
h_doner(no2,h_don0).
h_doner(br,h_don0).
h_doner(i,h_don0).
h_doner(cf3,h_don0).
h_doner(nbch3b2,h_don0).
h_doner(och3,h_don0).
h_doner(ocf3,h_don0).
h_doner(oso2ch3,h_don0).
h_doner(och2c6h5,h_don0).
h_doner(ch2och3,h_don0).
h_doner(och2ch2och3,h_don0).
h_doner(obch2b3ch3,h_don0).
h_doner(ch2obch2b3ch3,h_don0).
h_doner(obch2b5ch3,h_don0).
h_doner(obch2b6ch3,h_don0).
h_doner(obch2b7ch3,h_don0).
h_doner(nhcoch3,h_don1).
h_doner(och2conh2,h_don1).
h_doner(oh,h_don2).
h_doner(nh2,h_don2).
h_doner(ch2oh,h_don2).

h_doner(xf,h_don0).
h_doner(xch3,h_don0).
h_doner(xcl,h_don0).
h_doner(xno2,h_don0).
h_doner(xbr,h_don0).
h_doner(xi,h_don0).
h_doner(xcf3,h_don0).
h_doner(xnbch3b2,h_don0).
h_doner(xoch3,h_don0).
h_doner(xocf3,h_don0).
h_doner(xoso2ch3,h_don0).
h_doner(xoch2c6h5,h_don0).
h_doner(xch2och3,h_don0).
h_doner(xoch2ch2och3,h_don0).
h_doner(xobch2b3ch3,h_don0).
h_doner(xch2obch2b3ch3,h_don0).
h_doner(xobch2b5ch3,h_don0).
h_doner(xobch2b6ch3,h_don0).
h_doner(xobch2b7ch3,h_don0).
h_doner(xnhcoch3,h_don1).
h_doner(xoch2conh2,h_don1).
h_doner(xoh,h_don2).
h_doner(xnh2,h_don2).
h_doner(xch2oh,h_don2).

h_acceptor(ch3,h_acc0).
h_acceptor(cl,h_acc0).
h_acceptor(no2,h_acc0).
h_acceptor(br,h_acc0).
h_acceptor(i,h_acc0).
h_acceptor(cf3,h_acc0).
h_acceptor(ocf3,h_acc0).
h_acceptor(oso2ch3,h_acc0).
h_acceptor(ch2obch2b3ch3,h_acc0).
h_acceptor(nh2,h_acc0).
h_acceptor(f,h_acc1).
h_acceptor(nbch3b2,h_acc1).
h_acceptor(och3,h_acc1).
h_acceptor(nhcoch3,h_acc1).
h_acceptor(och2c6h5,h_acc1).
h_acceptor(ch2och3,h_acc1).
h_acceptor(och2ch2och3,h_acc1).
h_acceptor(obch2b3ch3,h_acc1).
h_acceptor(obch2b5ch3,h_acc1).
h_acceptor(obch2b6ch3,h_acc1).
h_acceptor(obch2b7ch3,h_acc1).
h_acceptor(och2conh2,h_acc1).
h_acceptor(oh,h_acc2).
h_acceptor(ch2oh,h_acc2).

h_acceptor(xch3,h_acc0).
h_acceptor(xcl,h_acc0).
h_acceptor(xno2,h_acc0).
h_acceptor(xbr,h_acc0).
h_acceptor(xi,h_acc0).
h_acceptor(xcf3,h_acc0).
h_acceptor(xocf3,h_acc0).
h_acceptor(xoso2ch3,h_acc0).
h_acceptor(xch2obch2b3ch3,h_acc0).
h_acceptor(xnh2,h_acc0).
h_acceptor(xf,h_acc1).
h_acceptor(xnbch3b2,h_acc1).
h_acceptor(xoch3,h_acc1).
h_acceptor(xnhcoch3,h_acc1).
h_acceptor(xoch2c6h5,h_acc1).
h_acceptor(xch2och3,h_acc1).
h_acceptor(xoch2ch2och3,h_acc1).
h_acceptor(xobch2b3ch3,h_acc1).
h_acceptor(xobch2b5ch3,h_acc1).
h_acceptor(xobch2b6ch3,h_acc1).
h_acceptor(xobch2b7ch3,h_acc1).
h_acceptor(xoch2conh2,h_acc1).
h_acceptor(xoh,h_acc2).
h_acceptor(xch2oh,h_acc2).

pi_doner(ch3,pi_don0).
pi_doner(no2,pi_don0).
pi_doner(cf3,pi_don0).
pi_doner(ocf3,pi_don0).
pi_doner(oso2ch3,pi_don0).
pi_doner(f,pi_don0).
pi_doner(ch2och3,pi_don0).
pi_doner(ch2obch2b3ch3,pi_don0).
pi_doner(ch2oh,pi_don0).
pi_doner(cl,pi_don0).
pi_doner(br,pi_don1).
pi_doner(i,pi_don1).
pi_doner(nhcoch3,pi_don1).
pi_doner(och2c6h5,pi_don1).
pi_doner(och2ch2och3,pi_don1).
pi_doner(obch2b3ch3,pi_don1).
pi_doner(obch2b5ch3,pi_don1).
pi_doner(obch2b6ch3,pi_don1).
pi_doner(obch2b7ch3,pi_don1).
pi_doner(och2conh2,pi_don1).
pi_doner(och3,pi_don1).
pi_doner(nh2,pi_don2).
pi_doner(nbch3b2,pi_don2).
pi_doner(oh,pi_don2).

pi_doner(xch3,pi_don0).
pi_doner(xno2,pi_don0).
pi_doner(xcf3,pi_don0).
pi_doner(xocf3,pi_don0).
pi_doner(xoso2ch3,pi_don0).
pi_doner(xf,pi_don0).
pi_doner(xch2och3,pi_don0).
pi_doner(xch2obch2b3ch3,pi_don0).
pi_doner(xch2oh,pi_don0).
pi_doner(xcl,pi_don0).
pi_doner(xbr,pi_don1).
pi_doner(xi,pi_don1).
pi_doner(xnhcoch3,pi_don1).
pi_doner(xoch2c6h5,pi_don1).
pi_doner(xoch2ch2och3,pi_don1).
pi_doner(xobch2b3ch3,pi_don1).
pi_doner(xobch2b5ch3,pi_don1).
pi_doner(xobch2b6ch3,pi_don1).
pi_doner(xobch2b7ch3,pi_don1).
pi_doner(xoch2conh2,pi_don1).
pi_doner(xoch3,pi_don1).
pi_doner(xnh2,pi_don2).
pi_doner(xnbch3b2,pi_don2).
pi_doner(xoh,pi_don2).

pi_acceptor(f,pi_acc0).
pi_acceptor(ch3,pi_acc0).
pi_acceptor(ch2och3,pi_acc0).
pi_acceptor(ch2obch2b3ch3,pi_acc0).
pi_acceptor(ch2oh,pi_acc0).
pi_acceptor(cl,pi_acc0).
pi_acceptor(br,pi_acc0).
pi_acceptor(nhcoch3,pi_acc0).
pi_acceptor(och2c6h5,pi_acc0).
pi_acceptor(och2ch2och3,pi_acc0).
pi_acceptor(obch2b3ch3,pi_acc0).
pi_acceptor(obch2b5ch3,pi_acc0).
pi_acceptor(obch2b6ch3,pi_acc0).
pi_acceptor(obch2b7ch3,pi_acc0).
pi_acceptor(och2conh2,pi_acc0).
pi_acceptor(nh2,pi_acc0).
pi_acceptor(nbch3b2,pi_acc0).
pi_acceptor(och3,pi_acc0).
pi_acceptor(oh,pi_acc0).
pi_acceptor(i,pi_acc0).
pi_acceptor(cf3,pi_acc0).
pi_acceptor(oso2ch3,pi_acc1).
pi_acceptor(no2,pi_acc2).
pi_acceptor(ocf3,pi_acc2).

pi_acceptor(xf,pi_acc0).
pi_acceptor(xch3,pi_acc0).
pi_acceptor(xch2och3,pi_acc0).
pi_acceptor(xch2obch2b3ch3,pi_acc0).
pi_acceptor(xch2oh,pi_acc0).
pi_acceptor(xcl,pi_acc0).
pi_acceptor(xbr,pi_acc0).
pi_acceptor(xnhcoch3,pi_acc0).
pi_acceptor(xoch2c6h5,pi_acc0).
pi_acceptor(xoch2ch2och3,pi_acc0).
pi_acceptor(xobch2b3ch3,pi_acc0).
pi_acceptor(xobch2b5ch3,pi_acc0).
pi_acceptor(xobch2b6ch3,pi_acc0).
pi_acceptor(xobch2b7ch3,pi_acc0).
pi_acceptor(xoch2conh2,pi_acc0).
pi_acceptor(xnh2,pi_acc0).
pi_acceptor(xnbch3b2,pi_acc0).
pi_acceptor(xoch3,pi_acc0).
pi_acceptor(xoh,pi_acc0).
pi_acceptor(xi,pi_acc0).
pi_acceptor(xcf3,pi_acc0).
pi_acceptor(xoso2ch3,pi_acc1).
pi_acceptor(xno2,pi_acc2).
pi_acceptor(xocf3,pi_acc2).

polarisable(och2conh2,polari0).
polarisable(nh2,polari0).
polarisable(cf3,polari0).
polarisable(no2,polari0).
polarisable(ocf3,polari0).
polarisable(f,polari0).
polarisable(ch3,polari1).
polarisable(ch2och3,polari1).
polarisable(ch2obch2b3ch3,polari1).
polarisable(ch2oh,polari1).
polarisable(cl,polari1).
polarisable(nhcoch3,polari1).
polarisable(och2ch2och3,polari1).
polarisable(obch2b3ch3,polari1).
polarisable(obch2b5ch3,polari1).
polarisable(obch2b6ch3,polari1).
polarisable(obch2b7ch3,polari1).
polarisable(nbch3b2,polari1).
polarisable(och3,polari1).
polarisable(oh,polari1).
polarisable(och2c6h5,polari1).
polarisable(br,polari2).
polarisable(oso2ch3,polari2).
polarisable(i,polari3).

polarisable(xoch2conh2,polari0).
polarisable(xnh2,polari0).
polarisable(xcf3,polari0).
polarisable(xno2,polari0).
polarisable(xocf3,polari0).
polarisable(xf,polari0).
polarisable(xch3,polari1).
polarisable(xch2och3,polari1).
polarisable(xch2obch2b3ch3,polari1).
polarisable(xch2oh,polari1).
polarisable(xcl,polari1).
polarisable(xnhcoch3,polari1).
polarisable(xoch2ch2och3,polari1).
polarisable(xobch2b3ch3,polari1).
polarisable(xobch2b5ch3,polari1).
polarisable(xobch2b6ch3,polari1).
polarisable(xobch2b7ch3,polari1).
polarisable(xnbch3b2,polari1).
polarisable(xoch3,polari1).
polarisable(xoh,polari1).
polarisable(xoch2c6h5,polari1).
polarisable(xbr,polari2).
polarisable(xoso2ch3,polari2).
polarisable(xi,polari3).

sigma(ch3,sigma0).
sigma(ch2och3,sigma0).
sigma(ch2obch2b3ch3,sigma0).
sigma(ch2oh,sigma0).
sigma(obch2b3ch3,sigma1).
sigma(nbch3b2,sigma1).
sigma(nh2,sigma1).
sigma(nhcoch3,sigma1).
sigma(och2ch2och3,sigma1).
sigma(obch2b5ch3,sigma1).
sigma(obch2b6ch3,sigma1).
sigma(obch2b7ch3,sigma1).
sigma(och3,sigma1).
sigma(och2c6h5,sigma1).
sigma(och2conh2,sigma1).
sigma(oh,sigma2).
sigma(i,sigma3).
sigma(br,sigma3).
sigma(oso2ch3,sigma3).
sigma(cl,sigma3).
sigma(cf3,sigma3).
sigma(no2,sigma3).
sigma(ocf3,sigma3).
sigma(f,sigma5).

sigma(xch3,sigma0).
sigma(xch2och3,sigma0).
sigma(xch2obch2b3ch3,sigma0).
sigma(xch2oh,sigma0).
sigma(xobch2b3ch3,sigma1).
sigma(xnbch3b2,sigma1).
sigma(xnh2,sigma1).
sigma(xnhcoch3,sigma1).
sigma(xoch2ch2och3,sigma1).
sigma(xobch2b5ch3,sigma1).
sigma(xobch2b6ch3,sigma1).
sigma(xobch2b7ch3,sigma1).
sigma(xoch3,sigma1).
sigma(xoch2c6h5,sigma1).
sigma(xoch2conh2,sigma1).
sigma(xoh,sigma2).
sigma(xi,sigma3).
sigma(xbr,sigma3).
sigma(xoso2ch3,sigma3).
sigma(xcl,sigma3).
sigma(xcf3,sigma3).
sigma(xno2,sigma3).
sigma(xocf3,sigma3).
sigma(xf,sigma5).

polar0(ch3).
polar0(ch2och3).
polar0(ch2obch2b3ch3).
polar1(nbch3b2).
polar2(ch2oh).
polar2(och3).
polar2(och2ch2och3).
polar2(obch2b3ch3).
polar2(obch2b5ch3).
polar2(obch2b6ch3).
polar2(obch2b7ch3).
polar2(och2c6h5).
polar3(nhcoch3).
polar3(och2conh2).
polar3(nh2).
polar3(i).
polar3(br).
polar3(cl).
polar3(oh).
polar3(cf3).
polar4(ocf3).
polar4(oso2ch3).
polar5(f).
polar5(no2).

size1(ch3).
size1(nh2).
size1(i).
size1(br).
size1(cl).
size1(oh).
size1(cf3).
size1(f).
size2(no2).
size2(nbch3b2).
size2(ch2och3).
size2(ch2oh).
size2(och3).
size2(nhcoch3).
size2(oso2ch3).
size3(ocf3).
size3(och2conh2).
size3(och2ch2och3).
size3(obch2b3ch3).
size4(ch2obch2b3ch3).
size4(och2c6h5).
size5(obch2b5ch3).
size5(obch2b6ch3).
size5(obch2b7ch3).

flex0(f).
flex0(oh).
flex0(nh2).
flex0(ch3).
flex0(cl).
flex0(no2).
flex0(br).
flex0(i).
flex0(cf3).
flex0(nbch3b2).
flex0(nhcoch3).
flex1(och3).
flex1(ocf3).
flex1(oso2ch3).
flex2(ch2oh).
flex2(och2c6h5).
flex2(och2conh2).
flex3(ch2och3).
flex4(och2ch2och3).
flex4(obch2b3ch3).
flex6(ch2obch2b3ch3).
flex6(obch2b5ch3).
flex7(obch2b6ch3).
flex8(obch2b7ch3).

h_doner0(f).
h_doner0(ch3).
h_doner0(cl).
h_doner0(no2).
h_doner0(br).
h_doner0(i).
h_doner0(cf3).
h_doner0(nbch3b2).
h_doner0(och3).
h_doner0(ocf3).
h_doner0(oso2ch3).
h_doner0(och2c6h5).
h_doner0(ch2och3).
h_doner0(och2ch2och3).
h_doner0(obch2b3ch3).
h_doner0(ch2obch2b3ch3).
h_doner0(obch2b5ch3).
h_doner0(obch2b6ch3).
h_doner0(obch2b7ch3).
h_doner1(nhcoch3).
h_doner1(och2conh2).
h_doner2(oh).
h_doner2(nh2).
h_doner2(ch2oh).

h_acceptor0(ch3).
h_acceptor0(cl).
h_acceptor0(no2).
h_acceptor0(br).
h_acceptor0(i).
h_acceptor0(cf3).
h_acceptor0(ocf3).
h_acceptor0(oso2ch3).
h_acceptor0(ch2obch2b3ch3).
h_acceptor0(nh2).
h_acceptor1(f).
h_acceptor1(nbch3b2).
h_acceptor1(och3).
h_acceptor1(nhcoch3).
h_acceptor1(och2c6h5).
h_acceptor1(ch2och3).
h_acceptor1(och2ch2och3).
h_acceptor1(obch2b3ch3).
h_acceptor1(obch2b5ch3).
h_acceptor1(obch2b6ch3).
h_acceptor1(obch2b7ch3).
h_acceptor1(och2conh2).
h_acceptor2(oh).
h_acceptor2(ch2oh).

pi_doner0(ch3).
pi_doner0(no2).
pi_doner0(cf3).
pi_doner0(ocf3).
pi_doner0(oso2ch3).
pi_doner0(f).
pi_doner0(ch2och3).
pi_doner0(ch2obch2b3ch3).
pi_doner0(ch2oh).
pi_doner0(cl).
pi_doner1(br).
pi_doner1(i).
pi_doner1(nhcoch3).
pi_doner1(och2c6h5).
pi_doner1(och2ch2och3).
pi_doner1(obch2b3ch3).
pi_doner1(obch2b5ch3).
pi_doner1(obch2b6ch3).
pi_doner1(obch2b7ch3).
pi_doner1(och2conh2).
pi_doner1(och3).
pi_doner2(nh2).
pi_doner2(nbch3b2).
pi_doner2(oh).

pi_acceptor0(f).
pi_acceptor0(ch3).
pi_acceptor0(ch2och3).
pi_acceptor0(ch2obch2b3ch3).
pi_acceptor0(ch2oh).
pi_acceptor0(cl).
pi_acceptor0(br).
pi_acceptor0(nhcoch3).
pi_acceptor0(och2c6h5).
pi_acceptor0(och2ch2och3).
pi_acceptor0(obch2b3ch3).
pi_acceptor0(obch2b5ch3).
pi_acceptor0(obch2b6ch3).
pi_acceptor0(obch2b7ch3).
pi_acceptor0(och2conh2).
pi_acceptor0(nh2).
pi_acceptor0(nbch3b2).
pi_acceptor0(och3).
pi_acceptor0(oh).
pi_acceptor0(i).
pi_acceptor0(cf3).
pi_acceptor1(oso2ch3).
pi_acceptor2(no2).
pi_acceptor2(ocf3).

polarisable0(och2conh2).
polarisable0(nh2).
polarisable0(cf3).
polarisable0(no2).
polarisable0(ocf3).
polarisable0(f).
polarisable1(ch3).
polarisable1(ch2och3).
polarisable1(ch2obch2b3ch3).
polarisable1(ch2oh).
polarisable1(cl).
polarisable1(nhcoch3).
polarisable1(och2ch2och3).
polarisable1(obch2b3ch3).
polarisable1(obch2b5ch3).
polarisable1(obch2b6ch3).
polarisable1(obch2b7ch3).
polarisable1(nbch3b2).
polarisable1(och3).
polarisable1(oh).
polarisable1(och2c6h5).
polarisable2(br).
polarisable2(oso2ch3).
polarisable3(i).

sigma0(ch3).
sigma0(ch2och3).
sigma0(ch2obch2b3ch3).
sigma0(ch2oh).
sigma1(obch2b3ch3).
sigma1(nbch3b2).
sigma1(nh2).
sigma1(nhcoch3).
sigma1(och2ch2och3).
sigma1(obch2b5ch3).
sigma1(obch2b6ch3).
sigma1(obch2b7ch3).
sigma1(och3).
sigma1(och2c6h5).
sigma1(och2conh2).
sigma2(oh).
sigma3(i).
sigma3(br).
sigma3(oso2ch3).
sigma3(cl).
sigma3(cf3).
sigma3(no2).
sigma3(ocf3).
sigma5(f).


polar0x(xch3).
polar0x(xch2och3).
polar0x(xch2obch2b3ch3).
polar1x(xnbch3b2).
polar2x(xch2oh).
polar2x(xoch3).
polar2x(xoch2ch2och3).
polar2x(xobch2b3ch3).
polar2x(xobch2b5ch3).
polar2x(xobch2b6ch3).
polar2x(xobch2b7ch3).
polar2x(xoch2c6h5).
polar3x(xnhcoch3).
polar3x(xoch2conh2).
polar3x(xnh2).
polar3x(xi).
polar3x(xbr).
polar3x(xcl).
polar3x(xoh).
polar3x(xcf3).
polar4x(xocf3).
polar4x(xoso2ch3).
polar5x(xf).
polar5x(xno2).

size1x(xch3).
size1x(xnh2).
size1x(xi).
size1x(xbr).
size1x(xcl).
size1x(xoh).
size1x(xcf3).
size1x(xf).
size2x(xno2).
size2x(xnbch3b2).
size2x(xch2och3).
size2x(xch2oh).
size2x(xoch3).
size2x(xnhcoch3).
size2x(xoso2ch3).
size3x(xocf3).
size3x(xoch2conh2).
size3x(xoch2ch2och3).
size3x(xobch2b3ch3).
size4x(xch2obch2b3ch3).
size4x(xoch2c6h5).
size5x(xobch2b5ch3).
size5x(xobch2b6ch3).
size5x(xobch2b7ch3).

flex0x(xf).
flex0x(xoh).
flex0x(xnh2).
flex0x(xch3).
flex0x(xcl).
flex0x(xno2).
flex0x(xbr).
flex0x(xi).
flex0x(xcf3).
flex0x(xnbch3b2).
flex0x(xnhcoch3).
flex1x(xoch3).
flex1x(xocf3).
flex1x(xoso2ch3).
flex2x(xch2oh).
flex2x(xoch2c6h5).
flex2x(xoch2conh2).
flex3x(xch2och3).
flex4x(xoch2ch2och3).
flex4x(xobch2b3ch3).
flex6x(xch2obch2b3ch3).
flex6x(xobch2b5ch3).
flex7x(xobch2b6ch3).
flex8x(xobch2b7ch3).

h_doner0x(xf).
h_doner0x(xch3).
h_doner0x(xcl).
h_doner0x(xno2).
h_doner0x(xbr).
h_doner0x(xi).
h_doner0x(xcf3).
h_doner0x(xnbch3b2).
h_doner0x(xoch3).
h_doner0x(xocf3).
h_doner0x(xoso2ch3).
h_doner0x(xoch2c6h5).
h_doner0x(xch2och3).
h_doner0x(xoch2ch2och3).
h_doner0x(xobch2b3ch3).
h_doner0x(xch2obch2b3ch3).
h_doner0x(xobch2b5ch3).
h_doner0x(xobch2b6ch3).
h_doner0x(xobch2b7ch3).
h_doner1x(xnhcoch3).
h_doner1x(xoch2conh2).
h_doner2x(xoh).
h_doner2x(xnh2).
h_doner2x(xch2oh).

h_acceptor0x(xch3).
h_acceptor0x(xcl).
h_acceptor0x(xno2).
h_acceptor0x(xbr).
h_acceptor0x(xi).
h_acceptor0x(xcf3).
h_acceptor0x(xocf3).
h_acceptor0x(xoso2ch3).
h_acceptor0x(xch2obch2b3ch3).
h_acceptor0x(xnh2).
h_acceptor1x(xf).
h_acceptor1x(xnbch3b2).
h_acceptor1x(xoch3).
h_acceptor1x(xnhcoch3).
h_acceptor1x(xoch2c6h5).
h_acceptor1x(xch2och3).
h_acceptor1x(xoch2ch2och3).
h_acceptor1x(xobch2b3ch3).
h_acceptor1x(xobch2b5ch3).
h_acceptor1x(xobch2b6ch3).
h_acceptor1x(xobch2b7ch3).
h_acceptor1x(xoch2conh2).
h_acceptor2x(xoh).
h_acceptor2x(xch2oh).

pi_doner0x(xch3).
pi_doner0x(xno2).
pi_doner0x(xcf3).
pi_doner0x(xocf3).
pi_doner0x(xoso2ch3).
pi_doner0x(xf).
pi_doner0x(xch2och3).
pi_doner0x(xch2obch2b3ch3).
pi_doner0x(xch2oh).
pi_doner0x(xcl).
pi_doner1x(xbr).
pi_doner1x(xi).
pi_doner1x(xnhcoch3).
pi_doner1x(xoch2c6h5).
pi_doner1x(xoch2ch2och3).
pi_doner1x(xobch2b3ch3).
pi_doner1x(xobch2b5ch3).
pi_doner1x(xobch2b6ch3).
pi_doner1x(xobch2b7ch3).
pi_doner1x(xoch2conh2).
pi_doner1x(xoch3).
pi_doner2x(xnh2).
pi_doner2x(xnbch3b2).
pi_doner2x(xoh).

pi_acceptor0x(xf).
pi_acceptor0x(xch3).
pi_acceptor0x(xch2och3).
pi_acceptor0x(xch2obch2b3ch3).
pi_acceptor0x(xch2oh).
pi_acceptor0x(xcl).
pi_acceptor0x(xbr).
pi_acceptor0x(xnhcoch3).
pi_acceptor0x(xoch2c6h5).
pi_acceptor0x(xoch2ch2och3).
pi_acceptor0x(xobch2b3ch3).
pi_acceptor0x(xobch2b5ch3).
pi_acceptor0x(xobch2b6ch3).
pi_acceptor0x(xobch2b7ch3).
pi_acceptor0x(xoch2conh2).
pi_acceptor0x(xnh2).
pi_acceptor0x(xnbch3b2).
pi_acceptor0x(xoch3).
pi_acceptor0x(xoh).
pi_acceptor0x(xi).
pi_acceptor0x(xcf3).
pi_acceptor1x(xoso2ch3).
pi_acceptor2x(xno2).
pi_acceptor2x(xocf3).

polarisable0x(xoch2conh2).
polarisable0x(xnh2).
polarisable0x(xcf3).
polarisable0x(xno2).
polarisable0x(xocf3).
polarisable0x(xf).
polarisable1x(xch3).
polarisable1x(xch2och3).
polarisable1x(xch2obch2b3ch3).
polarisable1x(xch2oh).
polarisable1x(xcl).
polarisable1x(xnhcoch3).
polarisable1x(xoch2ch2och3).
polarisable1x(xobch2b3ch3).
polarisable1x(xobch2b5ch3).
polarisable1x(xobch2b6ch3).
polarisable1x(xobch2b7ch3).
polarisable1x(xnbch3b2).
polarisable1x(xoch3).
polarisable1x(xoh).
polarisable1x(xoch2c6h5).
polarisable2x(xbr).
polarisable2x(xoso2ch3).
polarisable3x(xi).

sigma0x(xch3).
sigma0x(xch2och3).
sigma0x(xch2obch2b3ch3).
sigma0x(xch2oh).
sigma1x(xobch2b3ch3).
sigma1x(xnbch3b2).
sigma1x(xnh2).
sigma1x(xnhcoch3).
sigma1x(xoch2ch2och3).
sigma1x(xobch2b5ch3).
sigma1x(xobch2b6ch3).
sigma1x(xobch2b7ch3).
sigma1x(xoch3).
sigma1x(xoch2c6h5).
sigma1x(xoch2conh2).
sigma2x(xoh).
sigma3x(xi).
sigma3x(xbr).
sigma3x(xoso2ch3).
sigma3x(xcl).
sigma3x(xcf3).
sigma3x(xno2).
sigma3x(xocf3).
sigma5x(xf).


great_polar(polar1,polar0).
great_polar(polar2,polar0).
great_polar(polar3,polar0).
great_polar(polar4,polar0).
great_polar(polar5,polar0).
great_polar(polar6,polar0).
great_polar(polar7,polar0).
great_polar(polar8,polar0).
great_polar(polar9,polar0).
great_polar(polar2,polar1).
great_polar(polar3,polar1).
great_polar(polar4,polar1).
great_polar(polar5,polar1).
great_polar(polar6,polar1).
great_polar(polar7,polar1).
great_polar(polar8,polar1).
great_polar(polar9,polar1).
great_polar(polar3,polar2).
great_polar(polar4,polar2).
great_polar(polar5,polar2).
great_polar(polar6,polar2).
great_polar(polar7,polar2).
great_polar(polar8,polar2).
great_polar(polar9,polar2).
great_polar(polar4,polar3).
great_polar(polar5,polar3).
great_polar(polar6,polar3).
great_polar(polar7,polar3).
great_polar(polar8,polar3).
great_polar(polar9,polar3).
great_polar(polar5,polar4).
great_polar(polar6,polar4).
great_polar(polar7,polar4).
great_polar(polar8,polar4).
great_polar(polar9,polar4).
great_polar(polar6,polar5).
great_polar(polar7,polar5).
great_polar(polar8,polar5).
great_polar(polar9,polar5).
great_polar(polar7,polar6).
great_polar(polar8,polar6).
great_polar(polar9,polar6).
great_polar(polar8,polar7).
great_polar(polar9,polar7).
great_polar(polar9,polar8).

great0_polar(polar1).
great0_polar(polar2).
great0_polar(polar3).
great0_polar(polar4).
great0_polar(polar5).
great0_polar(polar6).
great0_polar(polar7).
great0_polar(polar8).
great0_polar(polar9).
great1_polar(polar2).
great1_polar(polar3).
great1_polar(polar4).
great1_polar(polar5).
great1_polar(polar6).
great1_polar(polar7).
great1_polar(polar8).
great1_polar(polar9).
great2_polar(polar3).
great2_polar(polar4).
great2_polar(polar5).
great2_polar(polar6).
great2_polar(polar7).
great2_polar(polar8).
great2_polar(polar9).
great3_polar(polar4).
great3_polar(polar5).
great3_polar(polar6).
great3_polar(polar7).
great3_polar(polar8).
great3_polar(polar9).
great4_polar(polar5).
great4_polar(polar6).
great4_polar(polar7).
great4_polar(polar8).
great4_polar(polar9).
great5_polar(polar6).
great5_polar(polar7).
great5_polar(polar8).
great5_polar(polar9).
great6_polar(polar7).
great6_polar(polar8).
great6_polar(polar9).
great7_polar(polar8).
great7_polar(polar9).

less9_polar(polar0).
less9_polar(polar1).
less9_polar(polar2).
less9_polar(polar3).
less9_polar(polar4).
less9_polar(polar5).
less9_polar(polar6).
less9_polar(polar7).
less9_polar(polar8).
less8_polar(polar0).
less8_polar(polar1).
less8_polar(polar2).
less8_polar(polar3).
less8_polar(polar4).
less8_polar(polar5).
less8_polar(polar6).
less8_polar(polar7).
less7_polar(polar0).
less7_polar(polar1).
less7_polar(polar2).
less7_polar(polar3).
less7_polar(polar4).
less7_polar(polar5).
less7_polar(polar6).
less6_polar(polar0).
less6_polar(polar1).
less6_polar(polar2).
less6_polar(polar3).
less6_polar(polar4).
less6_polar(polar5).
less5_polar(polar0).
less5_polar(polar1).
less5_polar(polar2).
less5_polar(polar3).
less5_polar(polar4).
less4_polar(polar0).
less4_polar(polar1).
less4_polar(polar2).
less4_polar(polar3).
less3_polar(polar0).
less3_polar(polar1).
less3_polar(polar2).
less2_polar(polar0).
less2_polar(polar1).

great_size(size1,size0).
great_size(size2,size0).
great_size(size3,size0).
great_size(size4,size0).
great_size(size5,size0).
great_size(size6,size0).
great_size(size7,size0).
great_size(size8,size0).
great_size(size9,size0).
great_size(size2,size1).
great_size(size3,size1).
great_size(size4,size1).
great_size(size5,size1).
great_size(size6,size1).
great_size(size7,size1).
great_size(size8,size1).
great_size(size9,size1).
great_size(size3,size2).
great_size(size4,size2).
great_size(size5,size2).
great_size(size6,size2).
great_size(size7,size2).
great_size(size8,size2).
great_size(size9,size2).
great_size(size4,size3).
great_size(size5,size3).
great_size(size6,size3).
great_size(size7,size3).
great_size(size8,size3).
great_size(size9,size3).
great_size(size5,size4).
great_size(size6,size4).
great_size(size7,size4).
great_size(size8,size4).
great_size(size9,size4).
great_size(size6,size5).
great_size(size7,size5).
great_size(size8,size5).
great_size(size9,size5).
great_size(size7,size6).
great_size(size8,size6).
great_size(size9,size6).
great_size(size8,size7).
great_size(size9,size7).
great_size(size9,size8).

great0_size(size1).
great0_size(size2).
great0_size(size3).
great0_size(size4).
great0_size(size5).
great0_size(size6).
great0_size(size7).
great0_size(size8).
great0_size(size9).
great1_size(size2).
great1_size(size3).
great1_size(size4).
great1_size(size5).
great1_size(size6).
great1_size(size7).
great1_size(size8).
great1_size(size9).
great2_size(size3).
great2_size(size4).
great2_size(size5).
great2_size(size6).
great2_size(size7).
great2_size(size8).
great2_size(size9).
great3_size(size4).
great3_size(size5).
great3_size(size6).
great3_size(size7).
great3_size(size8).
great3_size(size9).
great4_size(size5).
great4_size(size6).
great4_size(size7).
great4_size(size8).
great4_size(size9).
great5_size(size6).
great5_size(size7).
great5_size(size8).
great5_size(size9).
great6_size(size7).
great6_size(size8).
great6_size(size9).
great7_size(size8).
great7_size(size9).

less9_size(size0).
less9_size(size1).
less9_size(size2).
less9_size(size3).
less9_size(size4).
less9_size(size5).
less9_size(size6).
less9_size(size7).
less9_size(size8).
less8_size(size0).
less8_size(size1).
less8_size(size2).
less8_size(size3).
less8_size(size4).
less8_size(size5).
less8_size(size6).
less8_size(size7).
less7_size(size0).
less7_size(size1).
less7_size(size2).
less7_size(size3).
less7_size(size4).
less7_size(size5).
less7_size(size6).
less6_size(size0).
less6_size(size1).
less6_size(size2).
less6_size(size3).
less6_size(size4).
less6_size(size5).
less5_size(size0).
less5_size(size1).
less5_size(size2).
less5_size(size3).
less5_size(size4).
less4_size(size0).
less4_size(size1).
less4_size(size2).
less4_size(size3).
less3_size(size0).
less3_size(size1).
less3_size(size2).
less2_size(size0).
less2_size(size1).

great_flex(flex1,flex0).
great_flex(flex2,flex0).
great_flex(flex3,flex0).
great_flex(flex4,flex0).
great_flex(flex5,flex0).
great_flex(flex6,flex0).
great_flex(flex7,flex0).
great_flex(flex8,flex0).
great_flex(flex9,flex0).
great_flex(flex2,flex1).
great_flex(flex3,flex1).
great_flex(flex4,flex1).
great_flex(flex5,flex1).
great_flex(flex6,flex1).
great_flex(flex7,flex1).
great_flex(flex8,flex1).
great_flex(flex9,flex1).
great_flex(flex3,flex2).
great_flex(flex4,flex2).
great_flex(flex5,flex2).
great_flex(flex6,flex2).
great_flex(flex7,flex2).
great_flex(flex8,flex2).
great_flex(flex9,flex2).
great_flex(flex4,flex3).
great_flex(flex5,flex3).
great_flex(flex6,flex3).
great_flex(flex7,flex3).
great_flex(flex8,flex3).
great_flex(flex9,flex3).
great_flex(flex5,flex4).
great_flex(flex6,flex4).
great_flex(flex7,flex4).
great_flex(flex8,flex4).
great_flex(flex9,flex4).
great_flex(flex6,flex5).
great_flex(flex7,flex5).
great_flex(flex8,flex5).
great_flex(flex9,flex5).
great_flex(flex7,flex6).
great_flex(flex8,flex6).
great_flex(flex9,flex6).
great_flex(flex8,flex7).
great_flex(flex9,flex7).
great_flex(flex9,flex8).

great0_flex(flex1).
great0_flex(flex2).
great0_flex(flex3).
great0_flex(flex4).
great0_flex(flex5).
great0_flex(flex6).
great0_flex(flex7).
great0_flex(flex8).
great0_flex(flex9).
great1_flex(flex2).
great1_flex(flex3).
great1_flex(flex4).
great1_flex(flex5).
great1_flex(flex6).
great1_flex(flex7).
great1_flex(flex8).
great1_flex(flex9).
great2_flex(flex3).
great2_flex(flex4).
great2_flex(flex5).
great2_flex(flex6).
great2_flex(flex7).
great2_flex(flex8).
great2_flex(flex9).
great3_flex(flex4).
great3_flex(flex5).
great3_flex(flex6).
great3_flex(flex7).
great3_flex(flex8).
great3_flex(flex9).
great4_flex(flex5).
great4_flex(flex6).
great4_flex(flex7).
great4_flex(flex8).
great4_flex(flex9).
great5_flex(flex6).
great5_flex(flex7).
great5_flex(flex8).
great5_flex(flex9).
great6_flex(flex7).
great6_flex(flex8).
great6_flex(flex9).
great7_flex(flex8).
great7_flex(flex9).

less9_flex(flex0).
less9_flex(flex1).
less9_flex(flex2).
less9_flex(flex3).
less9_flex(flex4).
less9_flex(flex5).
less9_flex(flex6).
less9_flex(flex7).
less9_flex(flex8).
less8_flex(flex0).
less8_flex(flex1).
less8_flex(flex2).
less8_flex(flex3).
less8_flex(flex4).
less8_flex(flex5).
less8_flex(flex6).
less8_flex(flex7).
less7_flex(flex0).
less7_flex(flex1).
less7_flex(flex2).
less7_flex(flex3).
less7_flex(flex4).
less7_flex(flex5).
less7_flex(flex6).
less6_flex(flex0).
less6_flex(flex1).
less6_flex(flex2).
less6_flex(flex3).
less6_flex(flex4).
less6_flex(flex5).
less5_flex(flex0).
less5_flex(flex1).
less5_flex(flex2).
less5_flex(flex3).
less5_flex(flex4).
less4_flex(flex0).
less4_flex(flex1).
less4_flex(flex2).
less4_flex(flex3).
less3_flex(flex0).
less3_flex(flex1).
less3_flex(flex2).
less2_flex(flex0).
less2_flex(flex1).

great_h_don(h_don1,h_don0).
great_h_don(h_don2,h_don0).
great_h_don(h_don3,h_don0).
great_h_don(h_don4,h_don0).
great_h_don(h_don5,h_don0).
great_h_don(h_don6,h_don0).
great_h_don(h_don7,h_don0).
great_h_don(h_don8,h_don0).
great_h_don(h_don9,h_don0).
great_h_don(h_don2,h_don1).
great_h_don(h_don3,h_don1).
great_h_don(h_don4,h_don1).
great_h_don(h_don5,h_don1).
great_h_don(h_don6,h_don1).
great_h_don(h_don7,h_don1).
great_h_don(h_don8,h_don1).
great_h_don(h_don9,h_don1).
great_h_don(h_don3,h_don2).
great_h_don(h_don4,h_don2).
great_h_don(h_don5,h_don2).
great_h_don(h_don6,h_don2).
great_h_don(h_don7,h_don2).
great_h_don(h_don8,h_don2).
great_h_don(h_don9,h_don2).
great_h_don(h_don4,h_don3).
great_h_don(h_don5,h_don3).
great_h_don(h_don6,h_don3).
great_h_don(h_don7,h_don3).
great_h_don(h_don8,h_don3).
great_h_don(h_don9,h_don3).
great_h_don(h_don5,h_don4).
great_h_don(h_don6,h_don4).
great_h_don(h_don7,h_don4).
great_h_don(h_don8,h_don4).
great_h_don(h_don9,h_don4).
great_h_don(h_don6,h_don5).
great_h_don(h_don7,h_don5).
great_h_don(h_don8,h_don5).
great_h_don(h_don9,h_don5).
great_h_don(h_don7,h_don6).
great_h_don(h_don8,h_don6).
great_h_don(h_don9,h_don6).
great_h_don(h_don8,h_don7).
great_h_don(h_don9,h_don7).
great_h_don(h_don9,h_don8).

great0_h_don(h_don1).
great0_h_don(h_don2).
great0_h_don(h_don3).
great0_h_don(h_don4).
great0_h_don(h_don5).
great0_h_don(h_don6).
great0_h_don(h_don7).
great0_h_don(h_don8).
great0_h_don(h_don9).
great1_h_don(h_don2).
great1_h_don(h_don3).
great1_h_don(h_don4).
great1_h_don(h_don5).
great1_h_don(h_don6).
great1_h_don(h_don7).
great1_h_don(h_don8).
great1_h_don(h_don9).
great2_h_don(h_don3).
great2_h_don(h_don4).
great2_h_don(h_don5).
great2_h_don(h_don6).
great2_h_don(h_don7).
great2_h_don(h_don8).
great2_h_don(h_don9).
great3_h_don(h_don4).
great3_h_don(h_don5).
great3_h_don(h_don6).
great3_h_don(h_don7).
great3_h_don(h_don8).
great3_h_don(h_don9).
great4_h_don(h_don5).
great4_h_don(h_don6).
great4_h_don(h_don7).
great4_h_don(h_don8).
great4_h_don(h_don9).
great5_h_don(h_don6).
great5_h_don(h_don7).
great5_h_don(h_don8).
great5_h_don(h_don9).
great6_h_don(h_don7).
great6_h_don(h_don8).
great6_h_don(h_don9).
great7_h_don(h_don8).
great7_h_don(h_don9).

less9_h_don(h_don0).
less9_h_don(h_don1).
less9_h_don(h_don2).
less9_h_don(h_don3).
less9_h_don(h_don4).
less9_h_don(h_don5).
less9_h_don(h_don6).
less9_h_don(h_don7).
less9_h_don(h_don8).
less8_h_don(h_don0).
less8_h_don(h_don1).
less8_h_don(h_don2).
less8_h_don(h_don3).
less8_h_don(h_don4).
less8_h_don(h_don5).
less8_h_don(h_don6).
less8_h_don(h_don7).
less7_h_don(h_don0).
less7_h_don(h_don1).
less7_h_don(h_don2).
less7_h_don(h_don3).
less7_h_don(h_don4).
less7_h_don(h_don5).
less7_h_don(h_don6).
less6_h_don(h_don0).
less6_h_don(h_don1).
less6_h_don(h_don2).
less6_h_don(h_don3).
less6_h_don(h_don4).
less6_h_don(h_don5).
less5_h_don(h_don0).
less5_h_don(h_don1).
less5_h_don(h_don2).
less5_h_don(h_don3).
less5_h_don(h_don4).
less4_h_don(h_don0).
less4_h_don(h_don1).
less4_h_don(h_don2).
less4_h_don(h_don3).
less3_h_don(h_don0).
less3_h_don(h_don1).
less3_h_don(h_don2).
less2_h_don(h_don0).
less2_h_don(h_don1).

great_h_acc(h_acc1,h_acc0).
great_h_acc(h_acc2,h_acc0).
great_h_acc(h_acc3,h_acc0).
great_h_acc(h_acc4,h_acc0).
great_h_acc(h_acc5,h_acc0).
great_h_acc(h_acc6,h_acc0).
great_h_acc(h_acc7,h_acc0).
great_h_acc(h_acc8,h_acc0).
great_h_acc(h_acc9,h_acc0).
great_h_acc(h_acc2,h_acc1).
great_h_acc(h_acc3,h_acc1).
great_h_acc(h_acc4,h_acc1).
great_h_acc(h_acc5,h_acc1).
great_h_acc(h_acc6,h_acc1).
great_h_acc(h_acc7,h_acc1).
great_h_acc(h_acc8,h_acc1).
great_h_acc(h_acc9,h_acc1).
great_h_acc(h_acc3,h_acc2).
great_h_acc(h_acc4,h_acc2).
great_h_acc(h_acc5,h_acc2).
great_h_acc(h_acc6,h_acc2).
great_h_acc(h_acc7,h_acc2).
great_h_acc(h_acc8,h_acc2).
great_h_acc(h_acc9,h_acc2).
great_h_acc(h_acc4,h_acc3).
great_h_acc(h_acc5,h_acc3).
great_h_acc(h_acc6,h_acc3).
great_h_acc(h_acc7,h_acc3).
great_h_acc(h_acc8,h_acc3).
great_h_acc(h_acc9,h_acc3).
great_h_acc(h_acc5,h_acc4).
great_h_acc(h_acc6,h_acc4).
great_h_acc(h_acc7,h_acc4).
great_h_acc(h_acc8,h_acc4).
great_h_acc(h_acc9,h_acc4).
great_h_acc(h_acc6,h_acc5).
great_h_acc(h_acc7,h_acc5).
great_h_acc(h_acc8,h_acc5).
great_h_acc(h_acc9,h_acc5).
great_h_acc(h_acc7,h_acc6).
great_h_acc(h_acc8,h_acc6).
great_h_acc(h_acc9,h_acc6).
great_h_acc(h_acc8,h_acc7).
great_h_acc(h_acc9,h_acc7).
great_h_acc(h_acc9,h_acc8).

great0_h_acc(h_acc1).
great0_h_acc(h_acc2).
great0_h_acc(h_acc3).
great0_h_acc(h_acc4).
great0_h_acc(h_acc5).
great0_h_acc(h_acc6).
great0_h_acc(h_acc7).
great0_h_acc(h_acc8).
great0_h_acc(h_acc9).
great1_h_acc(h_acc2).
great1_h_acc(h_acc3).
great1_h_acc(h_acc4).
great1_h_acc(h_acc5).
great1_h_acc(h_acc6).
great1_h_acc(h_acc7).
great1_h_acc(h_acc8).
great1_h_acc(h_acc9).
great2_h_acc(h_acc3).
great2_h_acc(h_acc4).
great2_h_acc(h_acc5).
great2_h_acc(h_acc6).
great2_h_acc(h_acc7).
great2_h_acc(h_acc8).
great2_h_acc(h_acc9).
great3_h_acc(h_acc4).
great3_h_acc(h_acc5).
great3_h_acc(h_acc6).
great3_h_acc(h_acc7).
great3_h_acc(h_acc8).
great3_h_acc(h_acc9).
great4_h_acc(h_acc5).
great4_h_acc(h_acc6).
great4_h_acc(h_acc7).
great4_h_acc(h_acc8).
great4_h_acc(h_acc9).
great5_h_acc(h_acc6).
great5_h_acc(h_acc7).
great5_h_acc(h_acc8).
great5_h_acc(h_acc9).
great6_h_acc(h_acc7).
great6_h_acc(h_acc8).
great6_h_acc(h_acc9).
great7_h_acc(h_acc8).
great7_h_acc(h_acc9).

less9_h_acc(h_acc0).
less9_h_acc(h_acc1).
less9_h_acc(h_acc2).
less9_h_acc(h_acc3).
less9_h_acc(h_acc4).
less9_h_acc(h_acc5).
less9_h_acc(h_acc6).
less9_h_acc(h_acc7).
less9_h_acc(h_acc8).
less8_h_acc(h_acc0).
less8_h_acc(h_acc1).
less8_h_acc(h_acc2).
less8_h_acc(h_acc3).
less8_h_acc(h_acc4).
less8_h_acc(h_acc5).
less8_h_acc(h_acc6).
less8_h_acc(h_acc7).
less7_h_acc(h_acc0).
less7_h_acc(h_acc1).
less7_h_acc(h_acc2).
less7_h_acc(h_acc3).
less7_h_acc(h_acc4).
less7_h_acc(h_acc5).
less7_h_acc(h_acc6).
less6_h_acc(h_acc0).
less6_h_acc(h_acc1).
less6_h_acc(h_acc2).
less6_h_acc(h_acc3).
less6_h_acc(h_acc4).
less6_h_acc(h_acc5).
less5_h_acc(h_acc0).
less5_h_acc(h_acc1).
less5_h_acc(h_acc2).
less5_h_acc(h_acc3).
less5_h_acc(h_acc4).
less4_h_acc(h_acc0).
less4_h_acc(h_acc1).
less4_h_acc(h_acc2).
less4_h_acc(h_acc3).
less3_h_acc(h_acc0).
less3_h_acc(h_acc1).
less3_h_acc(h_acc2).
less2_h_acc(h_acc0).
less2_h_acc(h_acc1).

great_pi_don(pi_don1,pi_don0).
great_pi_don(pi_don2,pi_don0).
great_pi_don(pi_don3,pi_don0).
great_pi_don(pi_don4,pi_don0).
great_pi_don(pi_don5,pi_don0).
great_pi_don(pi_don6,pi_don0).
great_pi_don(pi_don7,pi_don0).
great_pi_don(pi_don8,pi_don0).
great_pi_don(pi_don9,pi_don0).
great_pi_don(pi_don2,pi_don1).
great_pi_don(pi_don3,pi_don1).
great_pi_don(pi_don4,pi_don1).
great_pi_don(pi_don5,pi_don1).
great_pi_don(pi_don6,pi_don1).
great_pi_don(pi_don7,pi_don1).
great_pi_don(pi_don8,pi_don1).
great_pi_don(pi_don9,pi_don1).
great_pi_don(pi_don3,pi_don2).
great_pi_don(pi_don4,pi_don2).
great_pi_don(pi_don5,pi_don2).
great_pi_don(pi_don6,pi_don2).
great_pi_don(pi_don7,pi_don2).
great_pi_don(pi_don8,pi_don2).
great_pi_don(pi_don9,pi_don2).
great_pi_don(pi_don4,pi_don3).
great_pi_don(pi_don5,pi_don3).
great_pi_don(pi_don6,pi_don3).
great_pi_don(pi_don7,pi_don3).
great_pi_don(pi_don8,pi_don3).
great_pi_don(pi_don9,pi_don3).
great_pi_don(pi_don5,pi_don4).
great_pi_don(pi_don6,pi_don4).
great_pi_don(pi_don7,pi_don4).
great_pi_don(pi_don8,pi_don4).
great_pi_don(pi_don9,pi_don4).
great_pi_don(pi_don6,pi_don5).
great_pi_don(pi_don7,pi_don5).
great_pi_don(pi_don8,pi_don5).
great_pi_don(pi_don9,pi_don5).
great_pi_don(pi_don7,pi_don6).
great_pi_don(pi_don8,pi_don6).
great_pi_don(pi_don9,pi_don6).
great_pi_don(pi_don8,pi_don7).
great_pi_don(pi_don9,pi_don7).
great_pi_don(pi_don9,pi_don8).

great0_pi_don(pi_don1).
great0_pi_don(pi_don2).
great0_pi_don(pi_don3).
great0_pi_don(pi_don4).
great0_pi_don(pi_don5).
great0_pi_don(pi_don6).
great0_pi_don(pi_don7).
great0_pi_don(pi_don8).
great0_pi_don(pi_don9).
great1_pi_don(pi_don2).
great1_pi_don(pi_don3).
great1_pi_don(pi_don4).
great1_pi_don(pi_don5).
great1_pi_don(pi_don6).
great1_pi_don(pi_don7).
great1_pi_don(pi_don8).
great1_pi_don(pi_don9).
great2_pi_don(pi_don3).
great2_pi_don(pi_don4).
great2_pi_don(pi_don5).
great2_pi_don(pi_don6).
great2_pi_don(pi_don7).
great2_pi_don(pi_don8).
great2_pi_don(pi_don9).
great3_pi_don(pi_don4).
great3_pi_don(pi_don5).
great3_pi_don(pi_don6).
great3_pi_don(pi_don7).
great3_pi_don(pi_don8).
great3_pi_don(pi_don9).
great4_pi_don(pi_don5).
great4_pi_don(pi_don6).
great4_pi_don(pi_don7).
great4_pi_don(pi_don8).
great4_pi_don(pi_don9).
great5_pi_don(pi_don6).
great5_pi_don(pi_don7).
great5_pi_don(pi_don8).
great5_pi_don(pi_don9).
great6_pi_don(pi_don7).
great6_pi_don(pi_don8).
great6_pi_don(pi_don9).
great7_pi_don(pi_don8).
great7_pi_don(pi_don9).

less9_pi_don(pi_don0).
less9_pi_don(pi_don1).
less9_pi_don(pi_don2).
less9_pi_don(pi_don3).
less9_pi_don(pi_don4).
less9_pi_don(pi_don5).
less9_pi_don(pi_don6).
less9_pi_don(pi_don7).
less9_pi_don(pi_don8).
less8_pi_don(pi_don0).
less8_pi_don(pi_don1).
less8_pi_don(pi_don2).
less8_pi_don(pi_don3).
less8_pi_don(pi_don4).
less8_pi_don(pi_don5).
less8_pi_don(pi_don6).
less8_pi_don(pi_don7).
less7_pi_don(pi_don0).
less7_pi_don(pi_don1).
less7_pi_don(pi_don2).
less7_pi_don(pi_don3).
less7_pi_don(pi_don4).
less7_pi_don(pi_don5).
less7_pi_don(pi_don6).
less6_pi_don(pi_don0).
less6_pi_don(pi_don1).
less6_pi_don(pi_don2).
less6_pi_don(pi_don3).
less6_pi_don(pi_don4).
less6_pi_don(pi_don5).
less5_pi_don(pi_don0).
less5_pi_don(pi_don1).
less5_pi_don(pi_don2).
less5_pi_don(pi_don3).
less5_pi_don(pi_don4).
less4_pi_don(pi_don0).
less4_pi_don(pi_don1).
less4_pi_don(pi_don2).
less4_pi_don(pi_don3).
less3_pi_don(pi_don0).
less3_pi_don(pi_don1).
less3_pi_don(pi_don2).
less2_pi_don(pi_don0).
less2_pi_don(pi_don1).

great_pi_acc(pi_acc1,pi_acc0).
great_pi_acc(pi_acc2,pi_acc0).
great_pi_acc(pi_acc3,pi_acc0).
great_pi_acc(pi_acc4,pi_acc0).
great_pi_acc(pi_acc5,pi_acc0).
great_pi_acc(pi_acc6,pi_acc0).
great_pi_acc(pi_acc7,pi_acc0).
great_pi_acc(pi_acc8,pi_acc0).
great_pi_acc(pi_acc9,pi_acc0).
great_pi_acc(pi_acc2,pi_acc1).
great_pi_acc(pi_acc3,pi_acc1).
great_pi_acc(pi_acc4,pi_acc1).
great_pi_acc(pi_acc5,pi_acc1).
great_pi_acc(pi_acc6,pi_acc1).
great_pi_acc(pi_acc7,pi_acc1).
great_pi_acc(pi_acc8,pi_acc1).
great_pi_acc(pi_acc9,pi_acc1).
great_pi_acc(pi_acc3,pi_acc2).
great_pi_acc(pi_acc4,pi_acc2).
great_pi_acc(pi_acc5,pi_acc2).
great_pi_acc(pi_acc6,pi_acc2).
great_pi_acc(pi_acc7,pi_acc2).
great_pi_acc(pi_acc8,pi_acc2).
great_pi_acc(pi_acc9,pi_acc2).
great_pi_acc(pi_acc4,pi_acc3).
great_pi_acc(pi_acc5,pi_acc3).
great_pi_acc(pi_acc6,pi_acc3).
great_pi_acc(pi_acc7,pi_acc3).
great_pi_acc(pi_acc8,pi_acc3).
great_pi_acc(pi_acc9,pi_acc3).
great_pi_acc(pi_acc5,pi_acc4).
great_pi_acc(pi_acc6,pi_acc4).
great_pi_acc(pi_acc7,pi_acc4).
great_pi_acc(pi_acc8,pi_acc4).
great_pi_acc(pi_acc9,pi_acc4).
great_pi_acc(pi_acc6,pi_acc5).
great_pi_acc(pi_acc7,pi_acc5).
great_pi_acc(pi_acc8,pi_acc5).
great_pi_acc(pi_acc9,pi_acc5).
great_pi_acc(pi_acc7,pi_acc6).
great_pi_acc(pi_acc8,pi_acc6).
great_pi_acc(pi_acc9,pi_acc6).
great_pi_acc(pi_acc8,pi_acc7).
great_pi_acc(pi_acc9,pi_acc7).
great_pi_acc(pi_acc9,pi_acc8).

great0_pi_acc(pi_acc1).
great0_pi_acc(pi_acc2).
great0_pi_acc(pi_acc3).
great0_pi_acc(pi_acc4).
great0_pi_acc(pi_acc5).
great0_pi_acc(pi_acc6).
great0_pi_acc(pi_acc7).
great0_pi_acc(pi_acc8).
great0_pi_acc(pi_acc9).
great1_pi_acc(pi_acc2).
great1_pi_acc(pi_acc3).
great1_pi_acc(pi_acc4).
great1_pi_acc(pi_acc5).
great1_pi_acc(pi_acc6).
great1_pi_acc(pi_acc7).
great1_pi_acc(pi_acc8).
great1_pi_acc(pi_acc9).
great2_pi_acc(pi_acc3).
great2_pi_acc(pi_acc4).
great2_pi_acc(pi_acc5).
great2_pi_acc(pi_acc6).
great2_pi_acc(pi_acc7).
great2_pi_acc(pi_acc8).
great2_pi_acc(pi_acc9).
great3_pi_acc(pi_acc4).
great3_pi_acc(pi_acc5).
great3_pi_acc(pi_acc6).
great3_pi_acc(pi_acc7).
great3_pi_acc(pi_acc8).
great3_pi_acc(pi_acc9).
great4_pi_acc(pi_acc5).
great4_pi_acc(pi_acc6).
great4_pi_acc(pi_acc7).
great4_pi_acc(pi_acc8).
great4_pi_acc(pi_acc9).
great5_pi_acc(pi_acc6).
great5_pi_acc(pi_acc7).
great5_pi_acc(pi_acc8).
great5_pi_acc(pi_acc9).
great6_pi_acc(pi_acc7).
great6_pi_acc(pi_acc8).
great6_pi_acc(pi_acc9).
great7_pi_acc(pi_acc8).
great7_pi_acc(pi_acc9).

less9_pi_acc(pi_acc0).
less9_pi_acc(pi_acc1).
less9_pi_acc(pi_acc2).
less9_pi_acc(pi_acc3).
less9_pi_acc(pi_acc4).
less9_pi_acc(pi_acc5).
less9_pi_acc(pi_acc6).
less9_pi_acc(pi_acc7).
less9_pi_acc(pi_acc8).
less8_pi_acc(pi_acc0).
less8_pi_acc(pi_acc1).
less8_pi_acc(pi_acc2).
less8_pi_acc(pi_acc3).
less8_pi_acc(pi_acc4).
less8_pi_acc(pi_acc5).
less8_pi_acc(pi_acc6).
less8_pi_acc(pi_acc7).
less7_pi_acc(pi_acc0).
less7_pi_acc(pi_acc1).
less7_pi_acc(pi_acc2).
less7_pi_acc(pi_acc3).
less7_pi_acc(pi_acc4).
less7_pi_acc(pi_acc5).
less7_pi_acc(pi_acc6).
less6_pi_acc(pi_acc0).
less6_pi_acc(pi_acc1).
less6_pi_acc(pi_acc2).
less6_pi_acc(pi_acc3).
less6_pi_acc(pi_acc4).
less6_pi_acc(pi_acc5).
less5_pi_acc(pi_acc0).
less5_pi_acc(pi_acc1).
less5_pi_acc(pi_acc2).
less5_pi_acc(pi_acc3).
less5_pi_acc(pi_acc4).
less4_pi_acc(pi_acc0).
less4_pi_acc(pi_acc1).
less4_pi_acc(pi_acc2).
less4_pi_acc(pi_acc3).
less3_pi_acc(pi_acc0).
less3_pi_acc(pi_acc1).
less3_pi_acc(pi_acc2).
less2_pi_acc(pi_acc0).
less2_pi_acc(pi_acc1).

great_polari(polari1,polari0).
great_polari(polari2,polari0).
great_polari(polari3,polari0).
great_polari(polari4,polari0).
great_polari(polari5,polari0).
great_polari(polari6,polari0).
great_polari(polari7,polari0).
great_polari(polari8,polari0).
great_polari(polari9,polari0).
great_polari(polari2,polari1).
great_polari(polari3,polari1).
great_polari(polari4,polari1).
great_polari(polari5,polari1).
great_polari(polari6,polari1).
great_polari(polari7,polari1).
great_polari(polari8,polari1).
great_polari(polari9,polari1).
great_polari(polari3,polari2).
great_polari(polari4,polari2).
great_polari(polari5,polari2).
great_polari(polari6,polari2).
great_polari(polari7,polari2).
great_polari(polari8,polari2).
great_polari(polari9,polari2).
great_polari(polari4,polari3).
great_polari(polari5,polari3).
great_polari(polari6,polari3).
great_polari(polari7,polari3).
great_polari(polari8,polari3).
great_polari(polari9,polari3).
great_polari(polari5,polari4).
great_polari(polari6,polari4).
great_polari(polari7,polari4).
great_polari(polari8,polari4).
great_polari(polari9,polari4).
great_polari(polari6,polari5).
great_polari(polari7,polari5).
great_polari(polari8,polari5).
great_polari(polari9,polari5).
great_polari(polari7,polari6).
great_polari(polari8,polari6).
great_polari(polari9,polari6).
great_polari(polari8,polari7).
great_polari(polari9,polari7).
great_polari(polari9,polari8).

great0_polari(polari1).
great0_polari(polari2).
great0_polari(polari3).
great0_polari(polari4).
great0_polari(polari5).
great0_polari(polari6).
great0_polari(polari7).
great0_polari(polari8).
great0_polari(polari9).
great1_polari(polari2).
great1_polari(polari3).
great1_polari(polari4).
great1_polari(polari5).
great1_polari(polari6).
great1_polari(polari7).
great1_polari(polari8).
great1_polari(polari9).
great2_polari(polari3).
great2_polari(polari4).
great2_polari(polari5).
great2_polari(polari6).
great2_polari(polari7).
great2_polari(polari8).
great2_polari(polari9).
great3_polari(polari4).
great3_polari(polari5).
great3_polari(polari6).
great3_polari(polari7).
great3_polari(polari8).
great3_polari(polari9).
great4_polari(polari5).
great4_polari(polari6).
great4_polari(polari7).
great4_polari(polari8).
great4_polari(polari9).
great5_polari(polari6).
great5_polari(polari7).
great5_polari(polari8).
great5_polari(polari9).
great6_polari(polari7).
great6_polari(polari8).
great6_polari(polari9).
great7_polari(polari8).
great7_polari(polari9).

less9_polari(polari0).
less9_polari(polari1).
less9_polari(polari2).
less9_polari(polari3).
less9_polari(polari4).
less9_polari(polari5).
less9_polari(polari6).
less9_polari(polari7).
less9_polari(polari8).
less8_polari(polari0).
less8_polari(polari1).
less8_polari(polari2).
less8_polari(polari3).
less8_polari(polari4).
less8_polari(polari5).
less8_polari(polari6).
less8_polari(polari7).
less7_polari(polari0).
less7_polari(polari1).
less7_polari(polari2).
less7_polari(polari3).
less7_polari(polari4).
less7_polari(polari5).
less7_polari(polari6).
less6_polari(polari0).
less6_polari(polari1).
less6_polari(polari2).
less6_polari(polari3).
less6_polari(polari4).
less6_polari(polari5).
less5_polari(polari0).
less5_polari(polari1).
less5_polari(polari2).
less5_polari(polari3).
less5_polari(polari4).
less4_polari(polari0).
less4_polari(polari1).
less4_polari(polari2).
less4_polari(polari3).
less3_polari(polari0).
less3_polari(polari1).
less3_polari(polari2).
less2_polari(polari0).
less2_polari(polari1).

great_sigma(sigma1,sigma0).
great_sigma(sigma2,sigma0).
great_sigma(sigma3,sigma0).
great_sigma(sigma4,sigma0).
great_sigma(sigma5,sigma0).
great_sigma(sigma6,sigma0).
great_sigma(sigma7,sigma0).
great_sigma(sigma8,sigma0).
great_sigma(sigma9,sigma0).
great_sigma(sigma2,sigma1).
great_sigma(sigma3,sigma1).
great_sigma(sigma4,sigma1).
great_sigma(sigma5,sigma1).
great_sigma(sigma6,sigma1).
great_sigma(sigma7,sigma1).
great_sigma(sigma8,sigma1).
great_sigma(sigma9,sigma1).
great_sigma(sigma3,sigma2).
great_sigma(sigma4,sigma2).
great_sigma(sigma5,sigma2).
great_sigma(sigma6,sigma2).
great_sigma(sigma7,sigma2).
great_sigma(sigma8,sigma2).
great_sigma(sigma9,sigma2).
great_sigma(sigma4,sigma3).
great_sigma(sigma5,sigma3).
great_sigma(sigma6,sigma3).
great_sigma(sigma7,sigma3).
great_sigma(sigma8,sigma3).
great_sigma(sigma9,sigma3).
great_sigma(sigma5,sigma4).
great_sigma(sigma6,sigma4).
great_sigma(sigma7,sigma4).
great_sigma(sigma8,sigma4).
great_sigma(sigma9,sigma4).
great_sigma(sigma6,sigma5).
great_sigma(sigma7,sigma5).
great_sigma(sigma8,sigma5).
great_sigma(sigma9,sigma5).
great_sigma(sigma7,sigma6).
great_sigma(sigma8,sigma6).
great_sigma(sigma9,sigma6).
great_sigma(sigma8,sigma7).
great_sigma(sigma9,sigma7).
great_sigma(sigma9,sigma8).

great0_sigma(sigma1).
great0_sigma(sigma2).
great0_sigma(sigma3).
great0_sigma(sigma4).
great0_sigma(sigma5).
great0_sigma(sigma6).
great0_sigma(sigma7).
great0_sigma(sigma8).
great0_sigma(sigma9).
great1_sigma(sigma2).
great1_sigma(sigma3).
great1_sigma(sigma4).
great1_sigma(sigma5).
great1_sigma(sigma6).
great1_sigma(sigma7).
great1_sigma(sigma8).
great1_sigma(sigma9).
great2_sigma(sigma3).
great2_sigma(sigma4).
great2_sigma(sigma5).
great2_sigma(sigma6).
great2_sigma(sigma7).
great2_sigma(sigma8).
great2_sigma(sigma9).
great3_sigma(sigma4).
great3_sigma(sigma5).
great3_sigma(sigma6).
great3_sigma(sigma7).
great3_sigma(sigma8).
great3_sigma(sigma9).
great4_sigma(sigma5).
great4_sigma(sigma6).
great4_sigma(sigma7).
great4_sigma(sigma8).
great4_sigma(sigma9).
great5_sigma(sigma6).
great5_sigma(sigma7).
great5_sigma(sigma8).
great5_sigma(sigma9).
great6_sigma(sigma7).
great6_sigma(sigma8).
great6_sigma(sigma9).
great7_sigma(sigma8).
great7_sigma(sigma9).

less9_sigma(sigma0).
less9_sigma(sigma1).
less9_sigma(sigma2).
less9_sigma(sigma3).
less9_sigma(sigma4).
less9_sigma(sigma5).
less9_sigma(sigma6).
less9_sigma(sigma7).
less9_sigma(sigma8).
less8_sigma(sigma0).
less8_sigma(sigma1).
less8_sigma(sigma2).
less8_sigma(sigma3).
less8_sigma(sigma4).
less8_sigma(sigma5).
less8_sigma(sigma6).
less8_sigma(sigma7).
less7_sigma(sigma0).
less7_sigma(sigma1).
less7_sigma(sigma2).
less7_sigma(sigma3).
less7_sigma(sigma4).
less7_sigma(sigma5).
less7_sigma(sigma6).
less6_sigma(sigma0).
less6_sigma(sigma1).
less6_sigma(sigma2).
less6_sigma(sigma3).
less6_sigma(sigma4).
less6_sigma(sigma5).
less5_sigma(sigma0).
less5_sigma(sigma1).
less5_sigma(sigma2).
less5_sigma(sigma3).
less5_sigma(sigma4).
less4_sigma(sigma0).
less4_sigma(sigma1).
less4_sigma(sigma2).
less4_sigma(sigma3).
less3_sigma(sigma0).
less3_sigma(sigma1).
less3_sigma(sigma2).
less2_sigma(sigma0).
less2_sigma(sigma1).

:-end_bg.

:-begin_in_pos.
great(d08,d01).
great(d11,d01).
great(d20,d01).
great(d23,d01).
great(d24,d01).
great(d30,d01).
great(d31,d01).
great(d34,d01).
great(d37,d01).
great(d39,d01).
great(d42,d01).
great(d08,d02).
great(d11,d02).
great(d20,d02).
great(d23,d02).
great(d24,d02).
great(d30,d02).
great(d31,d02).
great(d34,d02).
great(d37,d02).
great(d39,d02).
great(d42,d02).
great(d08,d03).
great(d11,d03).
great(d20,d03).
great(d23,d03).
great(d24,d03).
great(d30,d03).
great(d31,d03).
great(d34,d03).
great(d37,d03).
great(d39,d03).
great(d42,d03).
great(d08,d04).
great(d11,d04).
great(d20,d04).
great(d23,d04).
great(d24,d04).
great(d30,d04).
great(d31,d04).
great(d34,d04).
great(d37,d04).
great(d39,d04).
great(d42,d04).
great(d11,d05).
great(d20,d05).
great(d23,d05).
great(d24,d05).
great(d30,d05).
great(d31,d05).
great(d34,d05).
great(d37,d05).
great(d39,d05).
great(d42,d05).
great(d11,d06).
great(d20,d06).
great(d23,d06).
great(d24,d06).
great(d30,d06).
great(d31,d06).
great(d34,d06).
great(d37,d06).
great(d39,d06).
great(d42,d06).
great(d11,d07).
great(d20,d07).
great(d23,d07).
great(d24,d07).
great(d30,d07).
great(d31,d07).
great(d34,d07).
great(d37,d07).
great(d39,d07).
great(d42,d07).
great(d11,d08).
great(d12,d08).
great(d13,d08).
great(d14,d08).
great(d15,d08).
great(d16,d08).
great(d17,d08).
great(d18,d08).
great(d19,d08).
great(d20,d08).
great(d21,d08).
great(d22,d08).
great(d23,d08).
great(d24,d08).
great(d25,d08).
great(d26,d08).
great(d27,d08).
great(d28,d08).
great(d29,d08).
great(d30,d08).
great(d31,d08).
great(d32,d08).
great(d33,d08).
great(d34,d08).
great(d35,d08).
great(d36,d08).
great(d37,d08).
great(d38,d08).
great(d39,d08).
great(d40,d08).
great(d41,d08).
great(d42,d08).
great(d43,d08).
great(d44,d08).
great(d11,d09).
great(d20,d09).
great(d23,d09).
great(d24,d09).
great(d30,d09).
great(d31,d09).
great(d34,d09).
great(d37,d09).
great(d39,d09).
great(d42,d09).
great(d20,d10).
great(d23,d10).
great(d24,d10).
great(d30,d10).
great(d31,d10).
great(d34,d10).
great(d37,d10).
great(d39,d10).
great(d42,d10).
great(d14,d11).
great(d15,d11).
great(d16,d11).
great(d17,d11).
great(d18,d11).
great(d19,d11).
great(d20,d11).
great(d21,d11).
great(d22,d11).
great(d23,d11).
great(d24,d11).
great(d25,d11).
great(d26,d11).
great(d27,d11).
great(d28,d11).
great(d29,d11).
great(d30,d11).
great(d31,d11).
great(d32,d11).
great(d33,d11).
great(d34,d11).
great(d35,d11).
great(d36,d11).
great(d37,d11).
great(d38,d11).
great(d39,d11).
great(d40,d11).
great(d41,d11).
great(d42,d11).
great(d43,d11).
great(d44,d11).
great(d20,d12).
great(d23,d12).
great(d24,d12).
great(d30,d12).
great(d31,d12).
great(d34,d12).
great(d37,d12).
great(d39,d12).
great(d42,d12).
great(d20,d13).
great(d23,d13).
great(d24,d13).
great(d30,d13).
great(d31,d13).
great(d34,d13).
great(d37,d13).
great(d39,d13).
great(d42,d13).
great(d20,d14).
great(d23,d14).
great(d24,d14).
great(d30,d14).
great(d31,d14).
great(d34,d14).
great(d37,d14).
great(d39,d14).
great(d42,d14).
great(d23,d15).
great(d24,d15).
great(d30,d15).
great(d31,d15).
great(d34,d15).
great(d37,d15).
great(d39,d15).
great(d42,d15).
great(d20,d16).
great(d23,d16).
great(d24,d16).
great(d30,d16).
great(d31,d16).
great(d34,d16).
great(d37,d16).
great(d39,d16).
great(d42,d16).
great(d20,d17).
great(d23,d17).
great(d24,d17).
great(d30,d17).
great(d31,d17).
great(d34,d17).
great(d37,d17).
great(d39,d17).
great(d42,d17).
great(d23,d18).
great(d24,d18).
great(d30,d18).
great(d31,d18).
great(d34,d18).
great(d37,d18).
great(d39,d18).
great(d42,d18).
great(d23,d19).
great(d24,d19).
great(d30,d19).
great(d31,d19).
great(d34,d19).
great(d37,d19).
great(d39,d19).
great(d42,d19).
great(d23,d20).
great(d24,d20).
great(d25,d20).
great(d26,d20).
great(d27,d20).
great(d28,d20).
great(d29,d20).
great(d30,d20).
great(d31,d20).
great(d32,d20).
great(d33,d20).
great(d34,d20).
great(d35,d20).
great(d36,d20).
great(d37,d20).
great(d38,d20).
great(d39,d20).
great(d40,d20).
great(d41,d20).
great(d42,d20).
great(d43,d20).
great(d44,d20).
great(d23,d21).
great(d24,d21).
great(d30,d21).
great(d31,d21).
great(d34,d21).
great(d37,d21).
great(d39,d21).
great(d42,d21).
great(d23,d22).
great(d24,d22).
great(d30,d22).
great(d31,d22).
great(d34,d22).
great(d37,d22).
great(d39,d22).
great(d42,d22).
great(d24,d23).
great(d25,d23).
great(d26,d23).
great(d27,d23).
great(d28,d23).
great(d29,d23).
great(d30,d23).
great(d31,d23).
great(d32,d23).
great(d33,d23).
great(d34,d23).
great(d35,d23).
great(d36,d23).
great(d37,d23).
great(d38,d23).
great(d39,d23).
great(d40,d23).
great(d41,d23).
great(d42,d23).
great(d43,d23).
great(d44,d23).
great(d25,d24).
great(d26,d24).
great(d27,d24).
great(d28,d24).
great(d29,d24).
great(d30,d24).
great(d31,d24).
great(d32,d24).
great(d33,d24).
great(d34,d24).
great(d35,d24).
great(d36,d24).
great(d37,d24).
great(d38,d24).
great(d39,d24).
great(d40,d24).
great(d41,d24).
great(d42,d24).
great(d43,d24).
great(d44,d24).
great(d30,d25).
great(d31,d25).
great(d34,d25).
great(d37,d25).
great(d39,d25).
great(d42,d25).
great(d30,d26).
great(d31,d26).
great(d34,d26).
great(d37,d26).
great(d39,d26).
great(d42,d26).
great(d30,d27).
great(d31,d27).
great(d34,d27).
great(d37,d27).
great(d39,d27).
great(d42,d27).
great(d30,d28).
great(d31,d28).
great(d34,d28).
great(d37,d28).
great(d39,d28).
great(d42,d28).
great(d31,d29).
great(d34,d29).
great(d37,d29).
great(d39,d29).
great(d42,d29).
great(d34,d30).
great(d35,d30).
great(d36,d30).
great(d37,d30).
great(d38,d30).
great(d39,d30).
great(d40,d30).
great(d41,d30).
great(d42,d30).
great(d43,d30).
great(d44,d30).
great(d33,d31).
great(d34,d31).
great(d35,d31).
great(d36,d31).
great(d37,d31).
great(d38,d31).
great(d39,d31).
great(d40,d31).
great(d41,d31).
great(d42,d31).
great(d43,d31).
great(d44,d31).
great(d37,d32).
great(d39,d32).
great(d42,d32).
great(d37,d33).
great(d39,d33).
great(d42,d33).
great(d37,d34).
great(d38,d34).
great(d39,d34).
great(d40,d34).
great(d41,d34).
great(d42,d34).
great(d43,d34).
great(d44,d34).
great(d37,d35).
great(d39,d35).
great(d42,d35).
great(d39,d36).
great(d42,d36).
great(d38,d37).
great(d39,d37).
great(d40,d37).
great(d41,d37).
great(d42,d37).
great(d43,d37).
great(d44,d37).
great(d42,d38).
great(d40,d39).
great(d41,d39).
great(d42,d39).
great(d43,d39).
great(d44,d39).
great(d42,d40).
great(d42,d41).
great(d44,d42).
great(d45,d08).
great(d46,d08).
great(d47,d08).
great(d48,d08).
great(d49,d08).
great(d50,d08).
great(d51,d08).
great(d52,d08).
great(d53,d08).
great(d54,d08).
great(d55,d08).
great(d45,d11).
great(d46,d11).
great(d47,d11).
great(d48,d11).
great(d49,d11).
great(d50,d11).
great(d51,d11).
great(d52,d11).
great(d53,d11).
great(d54,d11).
great(d55,d11).
great(d45,d20).
great(d46,d20).
great(d47,d20).
great(d48,d20).
great(d49,d20).
great(d50,d20).
great(d51,d20).
great(d52,d20).
great(d53,d20).
great(d54,d20).
great(d55,d20).
great(d45,d23).
great(d46,d23).
great(d47,d23).
great(d48,d23).
great(d49,d23).
great(d50,d23).
great(d51,d23).
great(d52,d23).
great(d53,d23).
great(d54,d23).
great(d55,d23).
great(d45,d24).
great(d46,d24).
great(d47,d24).
great(d48,d24).
great(d49,d24).
great(d50,d24).
great(d51,d24).
great(d52,d24).
great(d53,d24).
great(d54,d24).
great(d55,d24).
great(d45,d30).
great(d46,d30).
great(d47,d30).
great(d48,d30).
great(d49,d30).
great(d50,d30).
great(d51,d30).
great(d52,d30).
great(d53,d30).
great(d54,d30).
great(d55,d30).
great(d45,d31).
great(d46,d31).
great(d47,d31).
great(d48,d31).
great(d49,d31).
great(d50,d31).
great(d51,d31).
great(d52,d31).
great(d53,d31).
great(d54,d31).
great(d55,d31).
great(d45,d34).
great(d46,d34).
great(d47,d34).
great(d48,d34).
great(d49,d34).
great(d50,d34).
great(d51,d34).
great(d52,d34).
great(d53,d34).
great(d54,d34).
great(d55,d34).
great(d45,d37).
great(d46,d37).
great(d47,d37).
great(d48,d37).
great(d49,d37).
great(d50,d37).
great(d51,d37).
great(d52,d37).
great(d53,d37).
great(d54,d37).
great(d55,d37).
great(d45,d39).
great(d46,d39).
great(d47,d39).
great(d48,d39).
great(d49,d39).
great(d50,d39).
great(d51,d39).
great(d52,d39).
great(d53,d39).
great(d54,d39).
great(d55,d39).
great(d47,d42).
great(d48,d42).
great(d49,d42).
great(d50,d42).
great(d51,d42).
great(d52,d42).
great(d42,d45).
great(d42,d46).
great(d42,d54).
great(d42,d55).
:-end_in_pos.
:-begin_in_neg.
great(d01,d08).
great(d02,d08).
great(d03,d08).
great(d04,d08).
great(d01,d11).
great(d02,d11).
great(d03,d11).
great(d04,d11).
great(d05,d11).
great(d06,d11).
great(d07,d11).
great(d08,d11).
great(d09,d11).
great(d08,d12).
great(d08,d13).
great(d08,d14).
great(d11,d14).
great(d08,d15).
great(d11,d15).
great(d08,d16).
great(d11,d16).
great(d08,d17).
great(d11,d17).
great(d08,d18).
great(d11,d18).
great(d08,d19).
great(d11,d19).
great(d01,d20).
great(d02,d20).
great(d03,d20).
great(d04,d20).
great(d05,d20).
great(d06,d20).
great(d07,d20).
great(d08,d20).
great(d09,d20).
great(d10,d20).
great(d11,d20).
great(d12,d20).
great(d13,d20).
great(d14,d20).
great(d16,d20).
great(d17,d20).
great(d08,d21).
great(d11,d21).
great(d08,d22).
great(d11,d22).
great(d01,d23).
great(d02,d23).
great(d03,d23).
great(d04,d23).
great(d05,d23).
great(d06,d23).
great(d07,d23).
great(d08,d23).
great(d09,d23).
great(d10,d23).
great(d11,d23).
great(d12,d23).
great(d13,d23).
great(d14,d23).
great(d15,d23).
great(d16,d23).
great(d17,d23).
great(d18,d23).
great(d19,d23).
great(d20,d23).
great(d21,d23).
great(d22,d23).
great(d01,d24).
great(d02,d24).
great(d03,d24).
great(d04,d24).
great(d05,d24).
great(d06,d24).
great(d07,d24).
great(d08,d24).
great(d09,d24).
great(d10,d24).
great(d11,d24).
great(d12,d24).
great(d13,d24).
great(d14,d24).
great(d15,d24).
great(d16,d24).
great(d17,d24).
great(d18,d24).
great(d19,d24).
great(d20,d24).
great(d21,d24).
great(d22,d24).
great(d23,d24).
great(d08,d25).
great(d11,d25).
great(d20,d25).
great(d23,d25).
great(d24,d25).
great(d08,d26).
great(d11,d26).
great(d20,d26).
great(d23,d26).
great(d24,d26).
great(d08,d27).
great(d11,d27).
great(d20,d27).
great(d23,d27).
great(d24,d27).
great(d08,d28).
great(d11,d28).
great(d20,d28).
great(d23,d28).
great(d24,d28).
great(d08,d29).
great(d11,d29).
great(d20,d29).
great(d23,d29).
great(d24,d29).
great(d01,d30).
great(d02,d30).
great(d03,d30).
great(d04,d30).
great(d05,d30).
great(d06,d30).
great(d07,d30).
great(d08,d30).
great(d09,d30).
great(d10,d30).
great(d11,d30).
great(d12,d30).
great(d13,d30).
great(d14,d30).
great(d15,d30).
great(d16,d30).
great(d17,d30).
great(d18,d30).
great(d19,d30).
great(d20,d30).
great(d21,d30).
great(d22,d30).
great(d23,d30).
great(d24,d30).
great(d25,d30).
great(d26,d30).
great(d27,d30).
great(d28,d30).
great(d01,d31).
great(d02,d31).
great(d03,d31).
great(d04,d31).
great(d05,d31).
great(d06,d31).
great(d07,d31).
great(d08,d31).
great(d09,d31).
great(d10,d31).
great(d11,d31).
great(d12,d31).
great(d13,d31).
great(d14,d31).
great(d15,d31).
great(d16,d31).
great(d17,d31).
great(d18,d31).
great(d19,d31).
great(d20,d31).
great(d21,d31).
great(d22,d31).
great(d23,d31).
great(d24,d31).
great(d25,d31).
great(d26,d31).
great(d27,d31).
great(d28,d31).
great(d29,d31).
great(d08,d32).
great(d11,d32).
great(d20,d32).
great(d23,d32).
great(d24,d32).
great(d08,d33).
great(d11,d33).
great(d20,d33).
great(d23,d33).
great(d24,d33).
great(d31,d33).
great(d01,d34).
great(d02,d34).
great(d03,d34).
great(d04,d34).
great(d05,d34).
great(d06,d34).
great(d07,d34).
great(d08,d34).
great(d09,d34).
great(d10,d34).
great(d11,d34).
great(d12,d34).
great(d13,d34).
great(d14,d34).
great(d15,d34).
great(d16,d34).
great(d17,d34).
great(d18,d34).
great(d19,d34).
great(d20,d34).
great(d21,d34).
great(d22,d34).
great(d23,d34).
great(d24,d34).
great(d25,d34).
great(d26,d34).
great(d27,d34).
great(d28,d34).
great(d29,d34).
great(d30,d34).
great(d31,d34).
great(d08,d35).
great(d11,d35).
great(d20,d35).
great(d23,d35).
great(d24,d35).
great(d30,d35).
great(d31,d35).
great(d08,d36).
great(d11,d36).
great(d20,d36).
great(d23,d36).
great(d24,d36).
great(d30,d36).
great(d31,d36).
great(d01,d37).
great(d02,d37).
great(d03,d37).
great(d04,d37).
great(d05,d37).
great(d06,d37).
great(d07,d37).
great(d08,d37).
great(d09,d37).
great(d10,d37).
great(d11,d37).
great(d12,d37).
great(d13,d37).
great(d14,d37).
great(d15,d37).
great(d16,d37).
great(d17,d37).
great(d18,d37).
great(d19,d37).
great(d20,d37).
great(d21,d37).
great(d22,d37).
great(d23,d37).
great(d24,d37).
great(d25,d37).
great(d26,d37).
great(d27,d37).
great(d28,d37).
great(d29,d37).
great(d30,d37).
great(d31,d37).
great(d32,d37).
great(d33,d37).
great(d34,d37).
great(d35,d37).
great(d08,d38).
great(d11,d38).
great(d20,d38).
great(d23,d38).
great(d24,d38).
great(d30,d38).
great(d31,d38).
great(d34,d38).
great(d37,d38).
great(d01,d39).
great(d02,d39).
great(d03,d39).
great(d04,d39).
great(d05,d39).
great(d06,d39).
great(d07,d39).
great(d08,d39).
great(d09,d39).
great(d10,d39).
great(d11,d39).
great(d12,d39).
great(d13,d39).
great(d14,d39).
great(d15,d39).
great(d16,d39).
great(d17,d39).
great(d18,d39).
great(d19,d39).
great(d20,d39).
great(d21,d39).
great(d22,d39).
great(d23,d39).
great(d24,d39).
great(d25,d39).
great(d26,d39).
great(d27,d39).
great(d28,d39).
great(d29,d39).
great(d30,d39).
great(d31,d39).
great(d32,d39).
great(d33,d39).
great(d34,d39).
great(d35,d39).
great(d36,d39).
great(d37,d39).
great(d08,d40).
great(d11,d40).
great(d20,d40).
great(d23,d40).
great(d24,d40).
great(d30,d40).
great(d31,d40).
great(d34,d40).
great(d37,d40).
great(d39,d40).
great(d08,d41).
great(d11,d41).
great(d20,d41).
great(d23,d41).
great(d24,d41).
great(d30,d41).
great(d31,d41).
great(d34,d41).
great(d37,d41).
great(d39,d41).
great(d01,d42).
great(d02,d42).
great(d03,d42).
great(d04,d42).
great(d05,d42).
great(d06,d42).
great(d07,d42).
great(d08,d42).
great(d09,d42).
great(d10,d42).
great(d11,d42).
great(d12,d42).
great(d13,d42).
great(d14,d42).
great(d15,d42).
great(d16,d42).
great(d17,d42).
great(d18,d42).
great(d19,d42).
great(d20,d42).
great(d21,d42).
great(d22,d42).
great(d23,d42).
great(d24,d42).
great(d25,d42).
great(d26,d42).
great(d27,d42).
great(d28,d42).
great(d29,d42).
great(d30,d42).
great(d31,d42).
great(d32,d42).
great(d33,d42).
great(d34,d42).
great(d35,d42).
great(d36,d42).
great(d37,d42).
great(d38,d42).
great(d39,d42).
great(d40,d42).
great(d41,d42).
great(d08,d43).
great(d11,d43).
great(d20,d43).
great(d23,d43).
great(d24,d43).
great(d30,d43).
great(d31,d43).
great(d34,d43).
great(d37,d43).
great(d39,d43).
great(d08,d44).
great(d11,d44).
great(d20,d44).
great(d23,d44).
great(d24,d44).
great(d30,d44).
great(d31,d44).
great(d34,d44).
great(d37,d44).
great(d39,d44).
great(d42,d44).
great(d45,d42).
great(d46,d42).
great(d54,d42).
great(d55,d42).
great(d08,d45).
great(d11,d45).
great(d20,d45).
great(d23,d45).
great(d24,d45).
great(d30,d45).
great(d31,d45).
great(d34,d45).
great(d37,d45).
great(d39,d45).
great(d08,d46).
great(d11,d46).
great(d20,d46).
great(d23,d46).
great(d24,d46).
great(d30,d46).
great(d31,d46).
great(d34,d46).
great(d37,d46).
great(d39,d46).
great(d08,d47).
great(d11,d47).
great(d20,d47).
great(d23,d47).
great(d24,d47).
great(d30,d47).
great(d31,d47).
great(d34,d47).
great(d37,d47).
great(d39,d47).
great(d42,d47).
great(d08,d48).
great(d11,d48).
great(d20,d48).
great(d23,d48).
great(d24,d48).
great(d30,d48).
great(d31,d48).
great(d34,d48).
great(d37,d48).
great(d39,d48).
great(d42,d48).
great(d08,d49).
great(d11,d49).
great(d20,d49).
great(d23,d49).
great(d24,d49).
great(d30,d49).
great(d31,d49).
great(d34,d49).
great(d37,d49).
great(d39,d49).
great(d42,d49).
great(d08,d50).
great(d11,d50).
great(d20,d50).
great(d23,d50).
great(d24,d50).
great(d30,d50).
great(d31,d50).
great(d34,d50).
great(d37,d50).
great(d39,d50).
great(d42,d50).
great(d08,d51).
great(d11,d51).
great(d20,d51).
great(d23,d51).
great(d24,d51).
great(d30,d51).
great(d31,d51).
great(d34,d51).
great(d37,d51).
great(d39,d51).
great(d42,d51).
great(d08,d52).
great(d11,d52).
great(d20,d52).
great(d23,d52).
great(d24,d52).
great(d30,d52).
great(d31,d52).
great(d34,d52).
great(d37,d52).
great(d39,d52).
great(d42,d52).
great(d08,d53).
great(d11,d53).
great(d20,d53).
great(d23,d53).
great(d24,d53).
great(d30,d53).
great(d31,d53).
great(d34,d53).
great(d37,d53).
great(d39,d53).
great(d08,d54).
great(d11,d54).
great(d20,d54).
great(d23,d54).
great(d24,d54).
great(d30,d54).
great(d31,d54).
great(d34,d54).
great(d37,d54).
great(d39,d54).
great(d08,d55).
great(d11,d55).
great(d20,d55).
great(d23,d55).
great(d24,d55).
great(d30,d55).
great(d31,d55).
great(d34,d55).
great(d37,d55).
great(d39,d55).
:-end_in_neg.