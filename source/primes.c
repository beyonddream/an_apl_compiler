# include "aplc.h"
int i_main[8] = {
	0, 1, 2, 0, 200, 200, 200, 200} 
;
main() {
	union mp_struct mp1, mp2, mp3, mp4, mp5, mp6, mp7, mp8, mp9, 
	mp10, mp11, mp12; 
	union res_struct res1, res2, res3, res4, res5, res6, res7, res8, res9, 
	res10, res11, res12, res13, res14, res15; 
	int i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, 
	i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, 
	i20, i21; 


	stmtno = 1;
	while (stmtno)
		switch(stmtno) {
		default: 
			stmtno = 0; 
			break;
		case 1: 
			stmtno = 1;
			trace("main", 1);
			i10 = 200;
			i5 = -1;
			i15 = 200;
			i14 = 200;
			i20 = 200;
			i3 = 0;
			{
				res15.i = 0;
				for (i3 = 0; i3 < i20; i3++) {
					res11.i = 0;
					i4 = (i15 - 1) * i14 + i3 % i14;
					for (i16 = i15 - 1; i16 >= 0; i16--) {
						if ((i4 / i10) != i5) {
							i5 = i4 / i10;
							(res6.i = (i5 + _ixorg));
						}
						i6 = i4 % i10;
						res11.i += (0 == ((i6 + _ixorg) % res6.i));
						i4 -= i14;
					}
					res15.i += (2 == res11.i);
				}
				printit(&res15, INT, 1);
			}
			memfree(&mp1.ip);
			stmtno = 0;
		}
}
