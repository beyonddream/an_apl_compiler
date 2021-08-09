# include "aplc.h"
extern struct trs_struct n;
int i_spiral[15] = {
	0, 1, 2, 4, 2, -1, 4, 0, 2, 1, 
	1, 2, 3, 2, 0} 
;
double r_spiral[1] = {
	0.5} 
;
spiral(z, _no2, l)
struct trs_struct *z, *_no2, *l;
{
	struct trs_struct e;
	struct trs_struct g;
	struct trs_struct c;
	struct trs_struct a;
	struct trs_struct trs1, trs2, trs3, trs4; 
	union mp_struct mp1, mp2, mp3, mp4, mp5, mp6, mp7, mp8, mp9, 
	mp10, mp11, mp12, mp13, mp14, mp15, mp16, mp17, mp18, mp19, 
	mp20, mp21, mp22, mp23, mp24, mp25, mp26, mp27, mp28, mp29, 
	mp30, mp31, mp32; 
	union res_struct res1, res2, res3, res4, res5, res6, res7, res8, res9, 
	res10, res11, res12, res13, res14, res15, res16, res17, res18, res19, 
	res20, res21, res22; 
	int i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, 
	i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, 
	i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, 
	i30, i31, i32, i33, i34, i35, i36, i37, i38, i39, 
	i40, i41, i42, i43; 

	e.type = UKTYPE;
	g.type = UKTYPE;
	c.type = UKTYPE;
	a.type = UKTYPE;

	stmtno = 1;
	while (stmtno)
		switch(stmtno) {
		default: 
			stmtno = 0; 
			break;
		case 1: 
			stmtno = 1;
			trace("spiral", 1);
			if (n.type == UKTYPE) error("undefined value used");
			valloc(&mp5, 1, INT);
			*mp5.ip = (*n.value.ip * 2);
			i6 = _ixorg;
			settrs(&trs1, INT, 1, mp5.ip);
			i1 = talloc(&trs1);
			mp1.ip = trs1.value.ip;
			for (i2 = 0; i2 < i1; i2++) {
				(*mp1.ip++  = i6++ );
			}
			assign(&a, &trs1);
			memfree(&mp5.ip);
		case 2: 
			stmtno = 2;
			trace("spiral", 2);
			if (a.type == UKTYPE) error("undefined value used");
			mp2.ip = a.value.ip;
			settrs(&trs2, INT, 1, a.shape);
			trs2.value.ip = mp2.ip;
			i7 = 1;
			i8 = *a.shape;
			i10 = *a.shape;
			valloc(&mp6, i10, INT);
			mp7 = mp6;
			for (i5 = 0; i5 < i10; i5++) {
				res5.i = 0;
				for (i9 = i5; i9 >= 0; i9--) {
					(res5.i = (*(a.value.ip + i9) - res5.i));
				}
				if (res5.i < 0)
					res5.i = - res5.i;
				(*mp6.ip++  = res5.i);
			}
			settrs(&trs3, INT, 1, a.shape);
			trs3.value.ip = mp7.ip;
			copies(&trs4, &trs2, &trs3);
			mp8.ip = trs4.value.ip;
			settrs(&trs1, INT, 1, trs4.shape);
			i1 = talloc(&trs1);
			mp1.ip = trs1.value.ip;
			for (i2 = 0; i2 < i1; i2++) {
				(*mp1.ip++  = (*mp8.ip++  % 4));
			}
			assign(&c, &trs1);
			memfree(&mp7.ip);
		case 3: 
			stmtno = 3;
			trace("spiral", 3);
			if (n.type == UKTYPE) error("undefined value used");
			settrs(&trs1, INT, 0, &i_spiral[1]);
			i1 = talloc(&trs1);
			mp1.ip = trs1.value.ip;
			{
				(*mp1.ip = ((int) floor((((double) *n.value.ip) / ((double) 2)))));
			}
			assign(&g, &trs1);
		case 4: 
			stmtno = 4;
			trace("spiral", 4);
			if (l->type == UKTYPE) error("undefined value used");
			valloc(&mp3, 1, INT);
			*mp3.ip = *l->shape;
			if (n.type == UKTYPE) error("undefined value used");
			i17 = 1;
			valloc(&mp15, i17, INT);
			mp16 = mp15;
			{
				(*mp15.ip = ((*n.value.ip * *n.value.ip) - 1));
			}
			if (c.type == UKTYPE) error("undefined value used");
			i8 = 1;
			valloc(&mp9, i8, INT);
			for (i9 = i8 - 1; i9 >= 0; i9--) {
				*(mp9.ip + i9) = iabs(*(mp16.ip + i9));
			}
			i10 = qsdalloc(1, &mp5, &mp6, &mp7);
			i9 = 0;
			{
				if (*(mp16.ip + i9) < 0)
					*(mp6.ip + i9) += *(c.shape + i9) + *(mp16.ip + i9);
			}
			i7 = accessor(1, mp9.ip, 1, c.shape, &mp8, mp5.ip, mp6.ip, mp7.ip);
			outershape(&mp24, 1, mp3.ip, 1, mp9.ip);
			i24 = *mp9.ip;
			settrs(&trs1, INT, 2, mp24.ip);
			i1 = talloc(&trs1);
			mp1.ip = trs1.value.ip;
			for (i2 = 0; i2 < i1; i2++) {
				i6 = i2 % i24;
				i8 = i6;
				i11 = i7;
				for (i9 = i10 - 1; i9 >= 0; i9--) {
					i11 += i8 * *(mp8.ip + i9);
					i8 /= *(mp9.ip + i9);
				}
				i4 = i2 / i24;
				i3 = (((*(c.value.ip + i11) + ((0 == *(c.value.ip + i11)) * 4)) + (i4 * *(l->shape + 1))) - _ixorg);
				(*mp1.ip++  = *(l->value.ip + (l->rank?i3:0)));
			}
			assign(&e, &trs1);
			memfree(&mp3.ip);
			memfree(&mp4.ip);
			memfree(&mp24.ip);
		case 5: 
			stmtno = 5;
			trace("spiral", 5);
			if (n.type == UKTYPE) error("undefined value used");
			i8 = 2;
			valloc(&mp6, i8, INT);
			mp7 = mp6;
			for (i3 = 0; i3 < i8; i3++) {
				(*mp6.ip++  = *n.value.ip);
			}
			trs2.type = UKTYPE;
			mp13.ip = &i_spiral[9];
			if (g.type == UKTYPE) error("undefined value used");
			if (e.type == UKTYPE) error("undefined value used");
			mp18.ip = e.value.ip;
			catshape(&mp16, 0, &i_spiral[1], 2, e.shape);
			i25 = 1;
			i28 = *(e.shape + 1);
			i27 = i25 + i28;
			i34 = *(mp16.ip + 1);
			i35 = i34;
			i36 = vsize(2, mp16.ip);
			valloc(&mp19, i36, INT);
			mp20 = mp19;
			for (i22 = 0; i22 < i36; i22++) {
				if (i35 >= i34) {
					res12.i = 0;
					i35 = 0;
				}
				i29 = i22 % i27;
				if (i29 < i25) {
					(res9.i = 0);
				}
				else {
					(res9.i = *mp18.ip++ );
				}
				res12.i += res9.i;
				i35++;
				(*mp19.ip++  = res12.i);
			}
			outershape(&mp25, 1, &i_spiral[13], 2, mp16.ip);
			i41 = vsize(2, mp16.ip);
			dtshape(&mp12, &i_spiral[9], 3, mp25.ip);
			i16 = qsdalloc(2, &mp8, &mp9, &mp10);
			dtmerge(&i_spiral[9], 3, &mp8, &mp9, &mp10);
			i13 = accessor(2, mp12.ip, 3, mp25.ip, &mp11, mp8.ip, mp9.ip, mp10.ip);
			i17 = i13;
			i42 = vsize(2, mp12.ip);
			valloc(&mp26, i42, INT);
			mp27 = mp26;
			for (i12 = 0; i12 < i42; i12++) {
				i21 = i17 - i41 * (i20 = i17 / i41);
				getmp(&res13, &mp20, i21, INT);
				cktype(&res13, INT, INT);
				(res6.i = (*g.value.ip + res13.i));
				i14 = *((i16 + mp12.ip) - 1);
				for (i15 = i16 - 1; i15 >= 0; i15--) {
					i17 += *(mp11.ip + i15);
					if (0 == ((i12 + 1) % i14))
						i14 *= *((i15 + mp12.ip) - 1);
					else
						break;
				}
				(*mp26.ip++  = res6.i);
			}
			settrs(&trs3, INT, 2, mp12.ip);
			trs3.value.ip = mp27.ip;
			linear(&trs4, &trs2, &trs3);
			mp28.ip = trs4.value.ip;
			aplsort(&mp31, &mp28, *trs4.shape, INT, 1);
			i10 = *trs4.shape;
			settrs(&trs1, INT, 2, mp7.ip);
			i1 = talloc(&trs1);
			mp1.ip = trs1.value.ip;
			for (i2 = 0; i2 < i1; i2++) {
				i9 = i2 % i10;
				(*mp1.ip++  = (*(mp31.ip + i9) + _ixorg));
			}
			assign( z, &trs1);
			memfree(&mp7.ip);
			memfree(&mp27.ip);
			memfree(&mp20.ip);
			memfree(&mp16.ip);
			memfree(&mp25.ip);
			memfree(&mp31.ip);
			stmtno = 0;
		}
}
int i_copies[5] = {
	0, 1, -1, 1, 0} 
;
copies(c, a, b)
struct trs_struct *c, *a, *b;
{
	struct trs_struct trs1; 
	union mp_struct mp1, mp2, mp3, mp4, mp5, mp6, mp7, mp8, mp9, 
	mp10, mp11, mp12, mp13, mp14, mp15, mp16, mp17, mp18, mp19, 
	mp20, mp21, mp22, mp23, mp24, mp25; 
	union res_struct res1, res2, res3, res4, res5, res6, res7, res8, res9, 
	res10, res11, res12, res13, res14, res15, res16, res17, res18, res19, 
	res20; 
	int i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, 
	i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, 
	i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, 
	i30, i31, i32, i33, i34, i35, i36, i37, i38, i39; 



	stmtno = 1;
	while (stmtno)
		switch(stmtno) {
		default: 
			stmtno = 0; 
			break;
		case 1: 
			stmtno = 1;
			trace("copies", 1);
			if (a->type == UKTYPE) error("undefined value used");
			mp2 = a->value;
			if (b->type == UKTYPE) error("undefined value used");
			mp3.ip = b->value.ip;
			i7 = *b->shape;
			res4.i = 0;
			for (i5 = 0; i5 < i7; i5++) {
				res4.i += *mp3.ip++ ;
			}
			valloc(&mp5, 1, INT);
			*mp5.ip = res4.i;
			i11 = _ixorg;
			catshape(&mp14, 0, &i_copies[1], 1, b->shape);
			i22 = 1;
			i25 = *b->shape;
			i24 = i22 + i25;
			i29 = 1;
			i30 = *mp14.ip;
			i14 = 1;
			valloc(&mp10, i14, INT);
			for (i15 = i14 - 1; i15 >= 0; i15--) {
				*(mp10.ip + i15) = *(mp14.ip + i15) - iabs(i_copies[(i15 + 2)]);
			}
			i16 = qsdalloc(1, &mp6, &mp7, &mp8);
			i13 = accessor(1, mp10.ip, 1, mp14.ip, &mp9, mp6.ip, mp7.ip, mp8.ip);
			i17 = i13;
			valloc(&mp19, 1, INT);
			i34 = *mp10.ip;
			*mp19.ip = i34;
			i35 = *mp19.ip;
			valloc(&mp20, i35, INT);
			mp21 = mp20;
			for (i12 = 0; i12 < i35; i12++) {
				res12.i = 0;
				for (i31 = i17; i31 >= 0; i31--) {
					i26 = i31;
					if (i26 < i22) {
						(res9.i = 0);
					}
					else {
						i26 -= i22;
						(res9.i = *(b->value.ip + i26));
					}
					res12.i += res9.i;
				}
				(res6.i = (res12.i + 1));
				i14 = *((i16 + mp10.ip) - 1);
				for (i15 = i16 - 1; i15 >= 0; i15--) {
					i17 += *(mp9.ip + i15);
					if (0 == ((i12 + 1) % i14))
						i14 *= *((i15 + mp10.ip) - 1);
					else
						break;
				}
				(*mp20.ip++  = res6.i);
			}
			aplsort(&mp22, &mp21, *mp19.ip, INT, 1);
			i37 = *mp5.ip;
			res19.i = 0;
			settrs(&trs1, a->type, 1, mp5.ip);
			i1 = talloc(&trs1);
			mp1 = trs1.value;
			for (i2 = 0; i2 < i1; i2++) {
				(res18.i = i11++ );
				res18.i = aplsearch(mp22.ip, &mp21, &res18, INT, *mp19.ip) < *mp19.ip;
				res19.i += res18.i;
				i3 = (res19.i - _ixorg);
				getmp(&res2, &mp2, i3, a->type);
				setmp(&res2, &mp1, i2, trs1.type);
			}
			assign( c, &trs1);
			memfree(&mp5.ip);
			memfree(&mp21.ip);
			memfree(&mp14.ip);
			memfree(&mp19.ip);
			memfree(&mp22.ip);
			memfree(&mp25.ip);
			stmtno = 0;
		}
}
int i_primes[5] = {
	0, 1, 2, 0, 1} 
;
primes(x, _no2, a)
struct trs_struct *x, *_no2, *a;
{
	struct trs_struct s;
	struct trs_struct trs1; 
	union mp_struct mp1, mp2, mp3, mp4, mp5, mp6, mp7, mp8, mp9, 
	mp10, mp11, mp12, mp13, mp14, mp15, mp16, mp17, mp18, mp19, 
	mp20, mp21, mp22; 
	union res_struct res1, res2, res3, res4, res5, res6, res7, res8, res9, 
	res10, res11, res12, res13, res14, res15, res16, res17, res18, res19, 
	res20, res21, res22; 
	int i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, 
	i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, 
	i20, i21, i22, i23, i24, i25, i26, i27; 

	s.type = UKTYPE;

	stmtno = 1;
	while (stmtno)
		switch(stmtno) {
		default: 
			stmtno = 0; 
			break;
		case 1: 
			stmtno = 1;
			trace("primes", 1);
			if (a->type == UKTYPE) error("undefined value used");
			valloc(&mp3, 1, INT);
			i4 = vsize(a->rank, a->shape);
			*mp3.ip = i4;
			mp5.ip = mp3.ip;
			i6 = 1;
			settrs(&trs1, INT, 0, &i_primes[1]);
			i1 = talloc(&trs1);
			mp1.ip = trs1.value.ip;
			{
				res3.i = 0;
				for (i3 = 0; i3 < i6; i3++) {
					res3.i += *mp5.ip++ ;
				}
				(*mp1.ip = res3.i);
			}
			assign(&s, &trs1);
			memfree(&mp3.ip);
		case 2: 
			stmtno = 2;
			trace("primes", 2);
			mp2.ip = a->value.ip;
			i23 = 0;
			if (s.type == UKTYPE) error("undefined value used");
			outershape(&mp7, 1, s.value.ip, 1, s.value.ip);
			i10 = *s.value.ip;
			i14 = *mp7.ip;
			i13 = *(mp7.ip + 1);
			i16 = (i14 - 1) * i13;
			i20 = 0;
			i22 = *(s.value.ip + i20);
			valloc(&mp13, i22, INT);
			for (i4 = 0; i4 < i22; i4++) {
				res12.i = 0;
				i5 = i16 + i4;
				for (i15 = i14 - 1; i15 >= 0; i15--) {
					i7 = i5 - i10 * (i6 = i5 / i10);
					res12.i += (0 == ((i7 + _ixorg) % (i6 + _ixorg)));
					i5 -= i13;
				}
				if (((2 == res12.i) != 0))
					*(mp13.ip + i23++) = i4;
			}
			memfree(&mp7.ip);
			valloc(&mp14, 1, INT);
			*mp14.ip = i23;
			valloc(&mp17, 1, INT);
			i26 = *mp14.ip;
			*mp17.ip = i26;
			i27 = *mp17.ip;
			valloc(&mp18, i27, INT);
			mp19 = mp18;
			for (i3 = 0; i3 < i27; i3++) {
				i24 = *(mp13.ip + i3);
				(*mp18.ip++  = (i24 + _ixorg));
			}
			aplsort(&mp20, &mp19, *mp17.ip, INT, 1);
			settrs(&trs1, BIT, a->rank, a->shape);
			i1 = talloc(&trs1);
			mp1.ip = trs1.value.ip;
			for (i2 = 0; i2 < i1; i2++) {
				(res22.i = *mp2.ip++ );
				res22.i = aplsearch(mp20.ip, &mp19, &res22, INT, *mp17.ip) < *mp17.ip;
				(*mp1.ip++  = res22.i);
			}
			assign( x, &trs1);
			memfree(&mp19.ip);
			memfree(&mp14.ip);
			memfree(&mp13.ip);
			memfree(&mp17.ip);
			memfree(&mp20.ip);
			stmtno = 0;
		}
}
int i_linear[4] = {
	0, 1, 1, 1} 
;
linear(l, _no2, m)
struct trs_struct *l, *_no2, *m;
{
	struct trs_struct trs1; 
	union mp_struct mp1, mp2, mp3, mp4, mp5, mp6, mp7, mp8; 
	union res_struct res1, res2, res3, res4, res5, res6, res7, res8, res9, 
	res10, res11, res12, res13, res14; 
	int i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, 
	i10, i11, i12, i13; 


	stmtno = 1;
	while (stmtno)
		switch(stmtno) {
		default: 
			stmtno = 0; 
			break;
		case 1: 
			stmtno = 1;
			trace("linear", 1);
			if (n.type == UKTYPE) error("undefined value used");
			if (m->type == UKTYPE) error("undefined value used");
			i3 = innershape(&mp2, 0, &i_linear[1], m->rank, m->shape);
			i9 = (m->rank?*m->shape:1);
			i10 = esubi(0, m->rank, m->shape);
			if (0 == m->rank) {
				i9 = 1;
			}
			i9--;
			i11 = i9 * i10;
			settrs(&trs1, INT, i3, mp2.ip);
			i1 = talloc(&trs1);
			mp1.ip = trs1.value.ip;
			for (i2 = 0; i2 < i1; i2++) {
				i6 = i2 - i10 * (i5 = i2 / i10);
				i6 += i11;
				res11.i = 0;
				res2.i = 1;
				for (i4 = i9; i4 >= 0; i4--) {
					(res3.i = (res2.i * (*(m->value.ip + (m->rank?i6:0)) - 1)));
					res2.i *= *n.value.ip;
					res11.i += res3.i;
					i6 -= i10;
				}
				(*mp1.ip++  = (res11.i + 1));
			}
			assign( l, &trs1);
			stmtno = 0;
		}
}
extern struct trs_struct y;
extern struct trs_struct x;
struct trs_struct x;
struct trs_struct y;
struct trs_struct n;
int i_main[16] = {
	0, 1, 2, 4, 2, -1, 0, 1, 0, 0, 
	1, 0, -1, 8, 2, 1} 
;
char c_main[] = " *";
main() {
	struct trs_struct trs1, trs2, trs3, trs4; 
	union mp_struct mp1, mp2, mp3, mp4, mp5, mp6, mp7, mp8; 
	union res_struct res1, res2, res3, res4, res5, res6, res7; 
	int i0, i1, i2, i3, i4, i5, i6, i7; 

	x.type = UKTYPE;
	y.type = UKTYPE;
	n.type = UKTYPE;

	stmtno = 1;
	while (stmtno)
		switch(stmtno) {
		default: 
			stmtno = 0; 
			break;
		case 1: 
			stmtno = 1;
			trace("main", 1);
			quad(stdin, &trs2);
			mp2 = trs2.value;
			settrs(&trs1, INT, 0, &i_main[1]);
			i1 = talloc(&trs1);
			mp1.ip = trs1.value.ip;
			for (i2 = 0; i2 < i1; i2++) {
				getmp(&res2, &mp2, ((trs2.rank) ? i2 : 0), trs2.type);
				cktype(&res2, INT, trs2.type);
				(*mp1.ip++  = res2.i);
			}
			assign(&n, &trs1);
		case 2: 
			stmtno = 2;
			trace("main", 2);
			trs2.type = UKTYPE;
			mp5.ip = &i_main[5];
			settrs(&trs3, INT, 2, &i_main[2]);
			trs3.value.ip = &i_main[5];
			spiral(&trs4, &trs2, &trs3);
			mp8 = trs4.value;
			i1 = vsize(trs4.rank, trs4.shape);
			for (i2 = 0; i2 < i1; i2++) {
				getmp(&res7, &mp8, i2, trs4.type);
				printit(&res7, trs4.type, bmpnl(i2, trs4.rank, trs4.shape));
			}
			assign(&y, &trs4);
		case 3: 
			stmtno = 3;
			trace("main", 3);
			trs2.type = UKTYPE;
			if (y.type == UKTYPE) error("undefined value used");
			mp2 = y.value;
			settrs(&trs3, y.type, y.rank, y.shape);
			trs3.value = mp2;
			primes(&trs4, &trs2, &trs3);
			mp5.ip = trs4.value.ip;
			i1 = vsize(trs4.rank, trs4.shape);
			for (i2 = 0; i2 < i1; i2++) {
				(res1.i = *mp5.ip++ );
				printit(&res1, INT, bmpnl(i2, trs4.rank, trs4.shape));
			}
			assign(&x, &trs4);
		case 4: 
			stmtno = 4;
			trace("main", 4);
			if (x.type == UKTYPE) error("undefined value used");
			mp2.ip = x.value.ip;
			i1 = vsize(x.rank, x.shape);
			for (i2 = 0; i2 < i1; i2++) {
				i3 = ((*mp2.ip++  + 1) - _ixorg);
				(res1.c = c_main[i3]);
				printit(&res1, CHAR, bmpnl(i2, x.rank, x.shape));
			}
			memfree(&mp5.ip);
			stmtno = 0;
		}
}
