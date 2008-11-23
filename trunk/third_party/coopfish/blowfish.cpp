/** @file
   Implementation of the BLOWFISH algorithm.
   Reference:  <http://www.counterpane.com/blowfish.html>
   @author     Ivan Vecerina - 2002 - This source file is hereby placed in the public domain
*/

#include "../../stdafx.h"

#include <exception>
#include "blowfish.h"


using blowfish::Pad;
using blowfish::Block;
using blowfish::kBlockSize;


namespace blowfish {

namespace core {


/** The standard initial pad used for Blowfish.
This pad is made out of the decimals of PI (calculated in binary).
The pad will be scrambled based on the provided password.
*/
Pad const defaultPiPad = {
 { // P
	0x243F6A88ul, 0x85A308D3ul, 0x13198A2Eul, 0x03707344ul,
	0xA4093822ul, 0x299F31D0ul, 0x082EFA98ul, 0xEC4E6C89ul,
	0x452821E6ul, 0x38D01377ul, 0xBE5466CFul, 0x34E90C6Cul,
	0xC0AC29B7ul, 0xC97C50DDul, 0x3F84D5B5ul, 0xB5470917ul,
	0x9216D5D9ul, 0x8979FB1Bul
 }, { // S
	//S[0]
	{0xD1310BA6ul, 0x98DFB5ACul, 0x2FFD72DBul, 0xD01ADFB7ul,
	 0xB8E1AFEDul, 0x6A267E96ul, 0xBA7C9045ul, 0xF12C7F99ul,
	 0x24A19947ul, 0xB3916CF7ul, 0x0801F2E2ul, 0x858EFC16ul,
	 0x636920D8ul, 0x71574E69ul, 0xA458FEA3ul, 0xF4933D7Eul,
	 0x0D95748Ful, 0x728EB658ul, 0x718BCD58ul, 0x82154AEEul,
	 0x7B54A41Dul, 0xC25A59B5ul, 0x9C30D539ul, 0x2AF26013ul,
	 0xC5D1B023ul, 0x286085F0ul, 0xCA417918ul, 0xB8DB38EFul,
	 0x8E79DCB0ul, 0x603A180Eul, 0x6C9E0E8Bul, 0xB01E8A3Eul,
	 0xD71577C1ul, 0xBD314B27ul, 0x78AF2FDAul, 0x55605C60ul,
	 0xE65525F3ul, 0xAA55AB94ul, 0x57489862ul, 0x63E81440ul,
	 0x55CA396Aul, 0x2AAB10B6ul, 0xB4CC5C34ul, 0x1141E8CEul,
	 0xA15486AFul, 0x7C72E993ul, 0xB3EE1411ul, 0x636FBC2Aul,
	 0x2BA9C55Dul, 0x741831F6ul, 0xCE5C3E16ul, 0x9B87931Eul,
	 0xAFD6BA33ul, 0x6C24CF5Cul, 0x7A325381ul, 0x28958677ul,
	 0x3B8F4898ul, 0x6B4BB9AFul, 0xC4BFE81Bul, 0x66282193ul,
	 0x61D809CCul, 0xFB21A991ul, 0x487CAC60ul, 0x5DEC8032ul,
	 0xEF845D5Dul, 0xE98575B1ul, 0xDC262302ul, 0xEB651B88ul,
	 0x23893E81ul, 0xD396ACC5ul, 0x0F6D6FF3ul, 0x83F44239ul,
	 0x2E0B4482ul, 0xA4842004ul, 0x69C8F04Aul, 0x9E1F9B5Eul,
	 0x21C66842ul, 0xF6E96C9Aul, 0x670C9C61ul, 0xABD388F0ul,
	 0x6A51A0D2ul, 0xD8542F68ul, 0x960FA728ul, 0xAB5133A3ul,
	 0x6EEF0B6Cul, 0x137A3BE4ul, 0xBA3BF050ul, 0x7EFB2A98ul,
	 0xA1F1651Dul, 0x39AF0176ul, 0x66CA593Eul, 0x82430E88ul,
	 0x8CEE8619ul, 0x456F9FB4ul, 0x7D84A5C3ul, 0x3B8B5EBEul,
	 0xE06F75D8ul, 0x85C12073ul, 0x401A449Ful, 0x56C16AA6ul,
	 0x4ED3AA62ul, 0x363F7706ul, 0x1BFEDF72ul, 0x429B023Dul,
	 0x37D0D724ul, 0xD00A1248ul, 0xDB0FEAD3ul, 0x49F1C09Bul,
	 0x075372C9ul, 0x80991B7Bul, 0x25D479D8ul, 0xF6E8DEF7ul,
	 0xE3FE501Aul, 0xB6794C3Bul, 0x976CE0BDul, 0x04C006BAul,
	 0xC1A94FB6ul, 0x409F60C4ul, 0x5E5C9EC2ul, 0x196A2463ul,
	 0x68FB6FAFul, 0x3E6C53B5ul, 0x1339B2EBul, 0x3B52EC6Ful,
	 0x6DFC511Ful, 0x9B30952Cul, 0xCC814544ul, 0xAF5EBD09ul,
	 0xBEE3D004ul, 0xDE334AFDul, 0x660F2807ul, 0x192E4BB3ul,
	 0xC0CBA857ul, 0x45C8740Ful, 0xD20B5F39ul, 0xB9D3FBDBul,
	 0x5579C0BDul, 0x1A60320Aul, 0xD6A100C6ul, 0x402C7279ul,
	 0x679F25FEul, 0xFB1FA3CCul, 0x8EA5E9F8ul, 0xDB3222F8ul,
	 0x3C7516DFul, 0xFD616B15ul, 0x2F501EC8ul, 0xAD0552ABul,
	 0x323DB5FAul, 0xFD238760ul, 0x53317B48ul, 0x3E00DF82ul,
	 0x9E5C57BBul, 0xCA6F8CA0ul, 0x1A87562Eul, 0xDF1769DBul,
	 0xD542A8F6ul, 0x287EFFC3ul, 0xAC6732C6ul, 0x8C4F5573ul,
	 0x695B27B0ul, 0xBBCA58C8ul, 0xE1FFA35Dul, 0xB8F011A0ul,
	 0x10FA3D98ul, 0xFD2183B8ul, 0x4AFCB56Cul, 0x2DD1D35Bul,
	 0x9A53E479ul, 0xB6F84565ul, 0xD28E49BCul, 0x4BFB9790ul,
	 0xE1DDF2DAul, 0xA4CB7E33ul, 0x62FB1341ul, 0xCEE4C6E8ul,
	 0xEF20CADAul, 0x36774C01ul, 0xD07E9EFEul, 0x2BF11FB4ul,
	 0x95DBDA4Dul, 0xAE909198ul, 0xEAAD8E71ul, 0x6B93D5A0ul,
	 0xD08ED1D0ul, 0xAFC725E0ul, 0x8E3C5B2Ful, 0x8E7594B7ul,
	 0x8FF6E2FBul, 0xF2122B64ul, 0x8888B812ul, 0x900DF01Cul,
	 0x4FAD5EA0ul, 0x688FC31Cul, 0xD1CFF191ul, 0xB3A8C1ADul,
	 0x2F2F2218ul, 0xBE0E1777ul, 0xEA752DFEul, 0x8B021FA1ul,
	 0xE5A0CC0Ful, 0xB56F74E8ul, 0x18ACF3D6ul, 0xCE89E299ul,
	 0xB4A84FE0ul, 0xFD13E0B7ul, 0x7CC43B81ul, 0xD2ADA8D9ul,
	 0x165FA266ul, 0x80957705ul, 0x93CC7314ul, 0x211A1477ul,
	 0xE6AD2065ul, 0x77B5FA86ul, 0xC75442F5ul, 0xFB9D35CFul,
	 0xEBCDAF0Cul, 0x7B3E89A0ul, 0xD6411BD3ul, 0xAE1E7E49ul,
	 0x00250E2Dul, 0x2071B35Eul, 0x226800BBul, 0x57B8E0AFul,
	 0x2464369Bul, 0xF009B91Eul, 0x5563911Dul, 0x59DFA6AAul,
	 0x78C14389ul, 0xD95A537Ful, 0x207D5BA2ul, 0x02E5B9C5ul,
	 0x83260376ul, 0x6295CFA9ul, 0x11C81968ul, 0x4E734A41ul,
	 0xB3472DCAul, 0x7B14A94Aul, 0x1B510052ul, 0x9A532915ul,
	 0xD60F573Ful, 0xBC9BC6E4ul, 0x2B60A476ul, 0x81E67400ul,
	 0x08BA6FB5ul, 0x571BE91Ful, 0xF296EC6Bul, 0x2A0DD915ul,
	 0xB6636521ul, 0xE7B9F9B6ul, 0xFF34052Eul, 0xC5855664ul,
	 0x53B02D5Dul, 0xA99F8FA1ul, 0x08BA4799ul, 0x6E85076Aul},
	//S[1]
	{0x4B7A70E9ul, 0xB5B32944ul, 0xDB75092Eul, 0xC4192623ul,
	 0xAD6EA6B0ul, 0x49A7DF7Dul, 0x9CEE60B8ul, 0x8FEDB266ul,
	 0xECAA8C71ul, 0x699A17FFul, 0x5664526Cul, 0xC2B19EE1ul,
	 0x193602A5ul, 0x75094C29ul, 0xA0591340ul, 0xE4183A3Eul,
	 0x3F54989Aul, 0x5B429D65ul, 0x6B8FE4D6ul, 0x99F73FD6ul,
	 0xA1D29C07ul, 0xEFE830F5ul, 0x4D2D38E6ul, 0xF0255DC1ul,
	 0x4CDD2086ul, 0x8470EB26ul, 0x6382E9C6ul, 0x021ECC5Eul,
	 0x09686B3Ful, 0x3EBAEFC9ul, 0x3C971814ul, 0x6B6A70A1ul,
	 0x687F3584ul, 0x52A0E286ul, 0xB79C5305ul, 0xAA500737ul,
	 0x3E07841Cul, 0x7FDEAE5Cul, 0x8E7D44ECul, 0x5716F2B8ul,
	 0xB03ADA37ul, 0xF0500C0Dul, 0xF01C1F04ul, 0x0200B3FFul,
	 0xAE0CF51Aul, 0x3CB574B2ul, 0x25837A58ul, 0xDC0921BDul,
	 0xD19113F9ul, 0x7CA92FF6ul, 0x94324773ul, 0x22F54701ul,
	 0x3AE5E581ul, 0x37C2DADCul, 0xC8B57634ul, 0x9AF3DDA7ul,
	 0xA9446146ul, 0x0FD0030Eul, 0xECC8C73Eul, 0xA4751E41ul,
	 0xE238CD99ul, 0x3BEA0E2Ful, 0x3280BBA1ul, 0x183EB331ul,
	 0x4E548B38ul, 0x4F6DB908ul, 0x6F420D03ul, 0xF60A04BFul,
	 0x2CB81290ul, 0x24977C79ul, 0x5679B072ul, 0xBCAF89AFul,
	 0xDE9A771Ful, 0xD9930810ul, 0xB38BAE12ul, 0xDCCF3F2Eul,
	 0x5512721Ful, 0x2E6B7124ul, 0x501ADDE6ul, 0x9F84CD87ul,
	 0x7A584718ul, 0x7408DA17ul, 0xBC9F9ABCul, 0xE94B7D8Cul,
	 0xEC7AEC3Aul, 0xDB851DFAul, 0x63094366ul, 0xC464C3D2ul,
	 0xEF1C1847ul, 0x3215D908ul, 0xDD433B37ul, 0x24C2BA16ul,
	 0x12A14D43ul, 0x2A65C451ul, 0x50940002ul, 0x133AE4DDul,
	 0x71DFF89Eul, 0x10314E55ul, 0x81AC77D6ul, 0x5F11199Bul,
	 0x043556F1ul, 0xD7A3C76Bul, 0x3C11183Bul, 0x5924A509ul,
	 0xF28FE6EDul, 0x97F1FBFAul, 0x9EBABF2Cul, 0x1E153C6Eul,
	 0x86E34570ul, 0xEAE96FB1ul, 0x860E5E0Aul, 0x5A3E2AB3ul,
	 0x771FE71Cul, 0x4E3D06FAul, 0x2965DCB9ul, 0x99E71D0Ful,
	 0x803E89D6ul, 0x5266C825ul, 0x2E4CC978ul, 0x9C10B36Aul,
	 0xC6150EBAul, 0x94E2EA78ul, 0xA5FC3C53ul, 0x1E0A2DF4ul,
	 0xF2F74EA7ul, 0x361D2B3Dul, 0x1939260Ful, 0x19C27960ul,
	 0x5223A708ul, 0xF71312B6ul, 0xEBADFE6Eul, 0xEAC31F66ul,
	 0xE3BC4595ul, 0xA67BC883ul, 0xB17F37D1ul, 0x018CFF28ul,
	 0xC332DDEFul, 0xBE6C5AA5ul, 0x65582185ul, 0x68AB9802ul,
	 0xEECEA50Ful, 0xDB2F953Bul, 0x2AEF7DADul, 0x5B6E2F84ul,
	 0x1521B628ul, 0x29076170ul, 0xECDD4775ul, 0x619F1510ul,
	 0x13CCA830ul, 0xEB61BD96ul, 0x0334FE1Eul, 0xAA0363CFul,
	 0xB5735C90ul, 0x4C70A239ul, 0xD59E9E0Bul, 0xCBAADE14ul,
	 0xEECC86BCul, 0x60622CA7ul, 0x9CAB5CABul, 0xB2F3846Eul,
	 0x648B1EAFul, 0x19BDF0CAul, 0xA02369B9ul, 0x655ABB50ul,
	 0x40685A32ul, 0x3C2AB4B3ul, 0x319EE9D5ul, 0xC021B8F7ul,
	 0x9B540B19ul, 0x875FA099ul, 0x95F7997Eul, 0x623D7DA8ul,
	 0xF837889Aul, 0x97E32D77ul, 0x11ED935Ful, 0x16681281ul,
	 0x0E358829ul, 0xC7E61FD6ul, 0x96DEDFA1ul, 0x7858BA99ul,
	 0x57F584A5ul, 0x1B227263ul, 0x9B83C3FFul, 0x1AC24696ul,
	 0xCDB30AEBul, 0x532E3054ul, 0x8FD948E4ul, 0x6DBC3128ul,
	 0x58EBF2EFul, 0x34C6FFEAul, 0xFE28ED61ul, 0xEE7C3C73ul,
	 0x5D4A14D9ul, 0xE864B7E3ul, 0x42105D14ul, 0x203E13E0ul,
	 0x45EEE2B6ul, 0xA3AAABEAul, 0xDB6C4F15ul, 0xFACB4FD0ul,
	 0xC742F442ul, 0xEF6ABBB5ul, 0x654F3B1Dul, 0x41CD2105ul,
	 0xD81E799Eul, 0x86854DC7ul, 0xE44B476Aul, 0x3D816250ul,
	 0xCF62A1F2ul, 0x5B8D2646ul, 0xFC8883A0ul, 0xC1C7B6A3ul,
	 0x7F1524C3ul, 0x69CB7492ul, 0x47848A0Bul, 0x5692B285ul,
	 0x095BBF00ul, 0xAD19489Dul, 0x1462B174ul, 0x23820E00ul,
	 0x58428D2Aul, 0x0C55F5EAul, 0x1DADF43Eul, 0x233F7061ul,
	 0x3372F092ul, 0x8D937E41ul, 0xD65FECF1ul, 0x6C223BDBul,
	 0x7CDE3759ul, 0xCBEE7460ul, 0x4085F2A7ul, 0xCE77326Eul,
	 0xA6078084ul, 0x19F8509Eul, 0xE8EFD855ul, 0x61D99735ul,
	 0xA969A7AAul, 0xC50C06C2ul, 0x5A04ABFCul, 0x800BCADCul,
	 0x9E447A2Eul, 0xC3453484ul, 0xFDD56705ul, 0x0E1E9EC9ul,
	 0xDB73DBD3ul, 0x105588CDul, 0x675FDA79ul, 0xE3674340ul,
	 0xC5C43465ul, 0x713E38D8ul, 0x3D28F89Eul, 0xF16DFF20ul,
	 0x153E21E7ul, 0x8FB03D4Aul, 0xE6E39F2Bul, 0xDB83ADF7ul},
	//S[2]
	{0xE93D5A68ul, 0x948140F7ul, 0xF64C261Cul, 0x94692934ul,
	 0x411520F7ul, 0x7602D4F7ul, 0xBCF46B2Eul, 0xD4A20068ul,
	 0xD4082471ul, 0x3320F46Aul, 0x43B7D4B7ul, 0x500061AFul,
	 0x1E39F62Eul, 0x97244546ul, 0x14214F74ul, 0xBF8B8840ul,
	 0x4D95FC1Dul, 0x96B591AFul, 0x70F4DDD3ul, 0x66A02F45ul,
	 0xBFBC09ECul, 0x03BD9785ul, 0x7FAC6DD0ul, 0x31CB8504ul,
	 0x96EB27B3ul, 0x55FD3941ul, 0xDA2547E6ul, 0xABCA0A9Aul,
	 0x28507825ul, 0x530429F4ul, 0x0A2C86DAul, 0xE9B66DFBul,
	 0x68DC1462ul, 0xD7486900ul, 0x680EC0A4ul, 0x27A18DEEul,
	 0x4F3FFEA2ul, 0xE887AD8Cul, 0xB58CE006ul, 0x7AF4D6B6ul,
	 0xAACE1E7Cul, 0xD3375FECul, 0xCE78A399ul, 0x406B2A42ul,
	 0x20FE9E35ul, 0xD9F385B9ul, 0xEE39D7ABul, 0x3B124E8Bul,
	 0x1DC9FAF7ul, 0x4B6D1856ul, 0x26A36631ul, 0xEAE397B2ul,
	 0x3A6EFA74ul, 0xDD5B4332ul, 0x6841E7F7ul, 0xCA7820FBul,
	 0xFB0AF54Eul, 0xD8FEB397ul, 0x454056ACul, 0xBA489527ul,
	 0x55533A3Aul, 0x20838D87ul, 0xFE6BA9B7ul, 0xD096954Bul,
	 0x55A867BCul, 0xA1159A58ul, 0xCCA92963ul, 0x99E1DB33ul,
	 0xA62A4A56ul, 0x3F3125F9ul, 0x5EF47E1Cul, 0x9029317Cul,
	 0xFDF8E802ul, 0x04272F70ul, 0x80BB155Cul, 0x05282CE3ul,
	 0x95C11548ul, 0xE4C66D22ul, 0x48C1133Ful, 0xC70F86DCul,
	 0x07F9C9EEul, 0x41041F0Ful, 0x404779A4ul, 0x5D886E17ul,
	 0x325F51EBul, 0xD59BC0D1ul, 0xF2BCC18Ful, 0x41113564ul,
	 0x257B7834ul, 0x602A9C60ul, 0xDFF8E8A3ul, 0x1F636C1Bul,
	 0x0E12B4C2ul, 0x02E1329Eul, 0xAF664FD1ul, 0xCAD18115ul,
	 0x6B2395E0ul, 0x333E92E1ul, 0x3B240B62ul, 0xEEBEB922ul,
	 0x85B2A20Eul, 0xE6BA0D99ul, 0xDE720C8Cul, 0x2DA2F728ul,
	 0xD0127845ul, 0x95B794FDul, 0x647D0862ul, 0xE7CCF5F0ul,
	 0x5449A36Ful, 0x877D48FAul, 0xC39DFD27ul, 0xF33E8D1Eul,
	 0x0A476341ul, 0x992EFF74ul, 0x3A6F6EABul, 0xF4F8FD37ul,
	 0xA812DC60ul, 0xA1EBDDF8ul, 0x991BE14Cul, 0xDB6E6B0Dul,
	 0xC67B5510ul, 0x6D672C37ul, 0x2765D43Bul, 0xDCD0E804ul,
	 0xF1290DC7ul, 0xCC00FFA3ul, 0xB5390F92ul, 0x690FED0Bul,
	 0x667B9FFBul, 0xCEDB7D9Cul, 0xA091CF0Bul, 0xD9155EA3ul,
	 0xBB132F88ul, 0x515BAD24ul, 0x7B9479BFul, 0x763BD6EBul,
	 0x37392EB3ul, 0xCC115979ul, 0x8026E297ul, 0xF42E312Dul,
	 0x6842ADA7ul, 0xC66A2B3Bul, 0x12754CCCul, 0x782EF11Cul,
	 0x6A124237ul, 0xB79251E7ul, 0x06A1BBE6ul, 0x4BFB6350ul,
	 0x1A6B1018ul, 0x11CAEDFAul, 0x3D25BDD8ul, 0xE2E1C3C9ul,
	 0x44421659ul, 0x0A121386ul, 0xD90CEC6Eul, 0xD5ABEA2Aul,
	 0x64AF674Eul, 0xDA86A85Ful, 0xBEBFE988ul, 0x64E4C3FEul,
	 0x9DBC8057ul, 0xF0F7C086ul, 0x60787BF8ul, 0x6003604Dul,
	 0xD1FD8346ul, 0xF6381FB0ul, 0x7745AE04ul, 0xD736FCCCul,
	 0x83426B33ul, 0xF01EAB71ul, 0xB0804187ul, 0x3C005E5Ful,
	 0x77A057BEul, 0xBDE8AE24ul, 0x55464299ul, 0xBF582E61ul,
	 0x4E58F48Ful, 0xF2DDFDA2ul, 0xF474EF38ul, 0x8789BDC2ul,
	 0x5366F9C3ul, 0xC8B38E74ul, 0xB475F255ul, 0x46FCD9B9ul,
	 0x7AEB2661ul, 0x8B1DDF84ul, 0x846A0E79ul, 0x915F95E2ul,
	 0x466E598Eul, 0x20B45770ul, 0x8CD55591ul, 0xC902DE4Cul,
	 0xB90BACE1ul, 0xBB8205D0ul, 0x11A86248ul, 0x7574A99Eul,
	 0xB77F19B6ul, 0xE0A9DC09ul, 0x662D09A1ul, 0xC4324633ul,
	 0xE85A1F02ul, 0x09F0BE8Cul, 0x4A99A025ul, 0x1D6EFE10ul,
	 0x1AB93D1Dul, 0x0BA5A4DFul, 0xA186F20Ful, 0x2868F169ul,
	 0xDCB7DA83ul, 0x573906FEul, 0xA1E2CE9Bul, 0x4FCD7F52ul,
	 0x50115E01ul, 0xA70683FAul, 0xA002B5C4ul, 0x0DE6D027ul,
	 0x9AF88C27ul, 0x773F8641ul, 0xC3604C06ul, 0x61A806B5ul,
	 0xF0177A28ul, 0xC0F586E0ul, 0x006058AAul, 0x30DC7D62ul,
	 0x11E69ED7ul, 0x2338EA63ul, 0x53C2DD94ul, 0xC2C21634ul,
	 0xBBCBEE56ul, 0x90BCB6DEul, 0xEBFC7DA1ul, 0xCE591D76ul,
	 0x6F05E409ul, 0x4B7C0188ul, 0x39720A3Dul, 0x7C927C24ul,
	 0x86E3725Ful, 0x724D9DB9ul, 0x1AC15BB4ul, 0xD39EB8FCul,
	 0xED545578ul, 0x08FCA5B5ul, 0xD83D7CD3ul, 0x4DAD0FC4ul,
	 0x1E50EF5Eul, 0xB161E6F8ul, 0xA28514D9ul, 0x6C51133Cul,
	 0x6FD5C7E7ul, 0x56E14EC4ul, 0x362ABFCEul, 0xDDC6C837ul,
	 0xD79A3234ul, 0x92638212ul, 0x670EFA8Eul, 0x406000E0ul},
	//S[3]
	{0x3A39CE37ul, 0xD3FAF5CFul, 0xABC27737ul, 0x5AC52D1Bul,
	 0x5CB0679Eul, 0x4FA33742ul, 0xD3822740ul, 0x99BC9BBEul,
	 0xD5118E9Dul, 0xBF0F7315ul, 0xD62D1C7Eul, 0xC700C47Bul,
	 0xB78C1B6Bul, 0x21A19045ul, 0xB26EB1BEul, 0x6A366EB4ul,
	 0x5748AB2Ful, 0xBC946E79ul, 0xC6A376D2ul, 0x6549C2C8ul,
	 0x530FF8EEul, 0x468DDE7Dul, 0xD5730A1Dul, 0x4CD04DC6ul,
	 0x2939BBDBul, 0xA9BA4650ul, 0xAC9526E8ul, 0xBE5EE304ul,
	 0xA1FAD5F0ul, 0x6A2D519Aul, 0x63EF8CE2ul, 0x9A86EE22ul,
	 0xC089C2B8ul, 0x43242EF6ul, 0xA51E03AAul, 0x9CF2D0A4ul,
	 0x83C061BAul, 0x9BE96A4Dul, 0x8FE51550ul, 0xBA645BD6ul,
	 0x2826A2F9ul, 0xA73A3AE1ul, 0x4BA99586ul, 0xEF5562E9ul,
	 0xC72FEFD3ul, 0xF752F7DAul, 0x3F046F69ul, 0x77FA0A59ul,
	 0x80E4A915ul, 0x87B08601ul, 0x9B09E6ADul, 0x3B3EE593ul,
	 0xE990FD5Aul, 0x9E34D797ul, 0x2CF0B7D9ul, 0x022B8B51ul,
	 0x96D5AC3Aul, 0x017DA67Dul, 0xD1CF3ED6ul, 0x7C7D2D28ul,
	 0x1F9F25CFul, 0xADF2B89Bul, 0x5AD6B472ul, 0x5A88F54Cul,
	 0xE029AC71ul, 0xE019A5E6ul, 0x47B0ACFDul, 0xED93FA9Bul,
	 0xE8D3C48Dul, 0x283B57CCul, 0xF8D56629ul, 0x79132E28ul,
	 0x785F0191ul, 0xED756055ul, 0xF7960E44ul, 0xE3D35E8Cul,
	 0x15056DD4ul, 0x88F46DBAul, 0x03A16125ul, 0x0564F0BDul,
	 0xC3EB9E15ul, 0x3C9057A2ul, 0x97271AECul, 0xA93A072Aul,
	 0x1B3F6D9Bul, 0x1E6321F5ul, 0xF59C66FBul, 0x26DCF319ul,
	 0x7533D928ul, 0xB155FDF5ul, 0x03563482ul, 0x8ABA3CBBul,
	 0x28517711ul, 0xC20AD9F8ul, 0xABCC5167ul, 0xCCAD925Ful,
	 0x4DE81751ul, 0x3830DC8Eul, 0x379D5862ul, 0x9320F991ul,
	 0xEA7A90C2ul, 0xFB3E7BCEul, 0x5121CE64ul, 0x774FBE32ul,
	 0xA8B6E37Eul, 0xC3293D46ul, 0x48DE5369ul, 0x6413E680ul,
	 0xA2AE0810ul, 0xDD6DB224ul, 0x69852DFDul, 0x09072166ul,
	 0xB39A460Aul, 0x6445C0DDul, 0x586CDECFul, 0x1C20C8AEul,
	 0x5BBEF7DDul, 0x1B588D40ul, 0xCCD2017Ful, 0x6BB4E3BBul,
	 0xDDA26A7Eul, 0x3A59FF45ul, 0x3E350A44ul, 0xBCB4CDD5ul,
	 0x72EACEA8ul, 0xFA6484BBul, 0x8D6612AEul, 0xBF3C6F47ul,
	 0xD29BE463ul, 0x542F5D9Eul, 0xAEC2771Bul, 0xF64E6370ul,
	 0x740E0D8Dul, 0xE75B1357ul, 0xF8721671ul, 0xAF537D5Dul,
	 0x4040CB08ul, 0x4EB4E2CCul, 0x34D2466Aul, 0x0115AF84ul,
	 0xE1B00428ul, 0x95983A1Dul, 0x06B89FB4ul, 0xCE6EA048ul,
	 0x6F3F3B82ul, 0x3520AB82ul, 0x011A1D4Bul, 0x277227F8ul,
	 0x611560B1ul, 0xE7933FDCul, 0xBB3A792Bul, 0x344525BDul,
	 0xA08839E1ul, 0x51CE794Bul, 0x2F32C9B7ul, 0xA01FBAC9ul,
	 0xE01CC87Eul, 0xBCC7D1F6ul, 0xCF0111C3ul, 0xA1E8AAC7ul,
	 0x1A908749ul, 0xD44FBD9Aul, 0xD0DADECBul, 0xD50ADA38ul,
	 0x0339C32Aul, 0xC6913667ul, 0x8DF9317Cul, 0xE0B12B4Ful,
	 0xF79E59B7ul, 0x43F5BB3Aul, 0xF2D519FFul, 0x27D9459Cul,
	 0xBF97222Cul, 0x15E6FC2Aul, 0x0F91FC71ul, 0x9B941525ul,
	 0xFAE59361ul, 0xCEB69CEBul, 0xC2A86459ul, 0x12BAA8D1ul,
	 0xB6C1075Eul, 0xE3056A0Cul, 0x10D25065ul, 0xCB03A442ul,
	 0xE0EC6E0Eul, 0x1698DB3Bul, 0x4C98A0BEul, 0x3278E964ul,
	 0x9F1F9532ul, 0xE0D392DFul, 0xD3A0342Bul, 0x8971F21Eul,
	 0x1B0A7441ul, 0x4BA3348Cul, 0xC5BE7120ul, 0xC37632D8ul,
	 0xDF359F8Dul, 0x9B992F2Eul, 0xE60B6F47ul, 0x0FE3F11Dul,
	 0xE54CDA54ul, 0x1EDAD891ul, 0xCE6279CFul, 0xCD3E7E6Ful,
	 0x1618B166ul, 0xFD2C1D05ul, 0x848FD2C5ul, 0xF6FB2299ul,
	 0xF523F357ul, 0xA6327623ul, 0x93A83531ul, 0x56CCCD02ul,
	 0xACF08162ul, 0x5A75EBB5ul, 0x6E163697ul, 0x88D273CCul,
	 0xDE966292ul, 0x81B949D0ul, 0x4C50901Bul, 0x71C65614ul,
	 0xE6C6C7BDul, 0x327A140Aul, 0x45E1D006ul, 0xC3F27B9Aul,
	 0xC9AA53FDul, 0x62A80F00ul, 0xBB25BFE2ul, 0x35BDD2F6ul,
	 0x71126905ul, 0xB2040222ul, 0xB6CBCF7Cul, 0xCD769C2Bul,
	 0x53113EC0ul, 0x1640E3D3ul, 0x38ABBD60ul, 0x2547ADF0ul,
	 0xBA38209Cul, 0xF746CE76ul, 0x77AFA1C5ul, 0x20756060ul,
	 0x85CBFE4Eul, 0x8AE88DD8ul, 0x7AAAF9B0ul, 0x4CF9AA7Eul,
	 0x1948C25Cul, 0x02FB8A8Cul, 0x01C36AE4ul, 0xD6EBE1F9ul,
	 0x90D4F869ul, 0xA65CDEA0ul, 0x3F09252Dul, 0xC208E69Ful,
	 0xB74E6132ul, 0xCE77E25Bul, 0x578FDFE3ul, 0x3AC372E6ul} }
};





/** Returns the end of buffer @a p of size @a len, but throws if @a len is incorrect.
An exception is thrown if its size is not a multiple of the Blowfish block size.
*/
static
uint8* checkedDataBufferEnd( uint8* p, size_t len )
{
   if( len % kBlockSize != 0 )
      throw std::exception( "Invalid buffer size was passed to a Blowfish encryption or decryption routine." );
   return p + len;
}

/** Iterator for traversing source or destination data as encryption blocks.
This class can be used like a pointer to an array of Block,
while providing an alignment-independent and endianess-independent behavior:
 - data is read/written as big-endian values regardless of the platform
 - data is read/written byte by byte, so it can operate on misaligned ranges

@warn
  - The interface provided is minimal, being mostly a subset of a standard forward-iterator.
  - The user is responsible for not modifying a const memory range.
    There is no const-iterator, and constness of the scanned range is disregarded internally.
*/
class BlockIterator
{
 public:
   /// Creates an iterator starting at a specified address. @note Constness is casted-off here.
   BlockIterator(void const* p) : p_( (uint8*)p ) {}

   /** Creates an iterator at the end of the memory starting at @a p and of length @a offsEnd.
   @note An exception is thrown if @a offsEnd is not a multiple of Blowfish::kBlockSize.
   */
   BlockIterator(void const* p, size_t offsEnd) : p_( checkedDataBufferEnd( (uint8*)p, offsEnd ) ) {}

   friend bool operator==(BlockIterator const& a, BlockIterator const& b) { return a.p_ == b.p_; } ///< Equality test.
   friend bool operator!=(BlockIterator const& a, BlockIterator const& b) { return a.p_ != b.p_; } ///< Inequality test.

   /// Advance the iterator to the next block - pre-increment operator.
   BlockIterator& operator++() { p_ += kBlockSize; return *this; }

   /// Dereferences the iterator. Returns *this to then provide custom Block setting and reading operators (a classic trick).
   BlockIterator& operator*() { return *this; }

   /// Assigns the value of @a block to the current location.
   void operator=( Block const& block )
   {
      uint32 const L( block.L ),  R( block.R );
      uint8* const p = p_;
      p[0] = uint8( L >> 24 );   p[1] = uint8( L >> 16 );   p[2] = uint8( L >> 8 );   p[3] = uint8( L );
      p[4] = uint8( R >> 24 );   p[5] = uint8( R >> 16 );   p[6] = uint8( R >> 8 );   p[7] = uint8( R );
   }

   /// Returns the contents of the current location as a Block structure.
   operator Block()
   {
      uint8 const* const p = p_;
      return Block
            (     (p[0]<<24) | (p[1]<<16) | (p[2]<<8) | p[3]
            ,     (p[4]<<24) | (p[5]<<16) | (p[6]<<8) | p[7]
            );
   }

 private:
   uint8*   p_;   ///< The beginning of the current encryption block in memory.
};






/// The base scrambling function of Blowfish, using the S boxes.
inline
uint32 F( Pad const& pad, uint32 u )
{
   uint8 const a = uint8(u>>24);
   uint8 const b = uint8(u>>16);
   uint8 const c = uint8(u>> 8);
   uint8 const d = uint8(u    );

   return (  ( pad.S[0][a] + pad.S[1][b] )  ^  pad.S[2][c]  )  +  pad.S[3][d];
}


/** Standard sixteen round enciphering of @a block using @a pad.
Initial and final x-oring of the data ( = whitening ) helps prevent some attacks.
*/
Block encipherBlock( Pad const& pad, Block const& block)
{
	uint32 L = block.L;
	uint32 R = block.R;
	L ^= pad.P[0];
	R ^= F(pad,L)^pad.P[1];  L ^= F(pad,R)^pad.P[2];
	R ^= F(pad,L)^pad.P[3];  L ^= F(pad,R)^pad.P[4];
	R ^= F(pad,L)^pad.P[5];  L ^= F(pad,R)^pad.P[6];
	R ^= F(pad,L)^pad.P[7];  L ^= F(pad,R)^pad.P[8];
	R ^= F(pad,L)^pad.P[9];  L ^= F(pad,R)^pad.P[10];
	R ^= F(pad,L)^pad.P[11]; L ^= F(pad,R)^pad.P[12];
	R ^= F(pad,L)^pad.P[13]; L ^= F(pad,R)^pad.P[14];
	R ^= F(pad,L)^pad.P[15]; L ^= F(pad,R)^pad.P[16];
	R ^= pad.P[17];
   return Block( R, L ); //! swapping L and R one more time... !
}


/// Standard sixteen round deciphering of @a block using @a pad
Block decipherBlock( Pad const& pad, Block const& block)
{
	uint32 L = block.L;
	uint32 R = block.R;
	L ^= pad.P[17];
	R ^= F(pad,L)^pad.P[16]; L ^= F(pad,R)^pad.P[15];
	R ^= F(pad,L)^pad.P[14]; L ^= F(pad,R)^pad.P[13];
	R ^= F(pad,L)^pad.P[12]; L ^= F(pad,R)^pad.P[11];
	R ^= F(pad,L)^pad.P[10]; L ^= F(pad,R)^pad.P[9];
	R ^= F(pad,L)^pad.P[8];  L ^= F(pad,R)^pad.P[7];
	R ^= F(pad,L)^pad.P[6];  L ^= F(pad,R)^pad.P[5];
	R ^= F(pad,L)^pad.P[4];  L ^= F(pad,R)^pad.P[3];
	R ^= F(pad,L)^pad.P[2];  L ^= F(pad,R)^pad.P[1];
	R ^= pad.P[0];
   return Block( R, L ); //! swapped L and R !
}



}//namespace core






Pad generatePad( void const* const keyPtr, int const keyLen )
{
   if( keyLen < 1  ||  keyLen > 56  )
	   throw std::exception("Invalid key length used to initialize BlowFish.");

   Pad ans = core::defaultPiPad;

   // Load P boxes with key bytes, by xor-ing the entire array with key data (which is cycled through).
   unsigned char const* const keyBase = reinterpret_cast<const uint8*>(keyPtr);
   unsigned char const* const keyEnd  = keyBase + keyLen;
   unsigned char const*       keyScan = keyBase;
   uint32*       boxScan = ans.P;
   uint32* const boxEnd  = ans.P+18;
   do {
      uint32 word = 0; // need to work on a block-by-block basis, as endianness within the P buffer may vary
      for( unsigned i = 4 ; i ; --i )
      {
         word = (word<<8) | *keyScan++;
         if( keyScan == keyEnd ) keyScan = keyBase;
      }
      *boxScan++ ^= word;
   } while( boxScan != boxEnd );

   // Use blowfish to reflectively scramble P and S boxes, while evolving the Blowfish pad.
   Block block(0,0); //zeroed start block
   uint32*       scanPad = reinterpret_cast<uint32*>( &ans );
   uint32* const endPad  = reinterpret_cast<uint32*>( &ans + 1 );
   do {
      block = core::encipherBlock( ans, block );
      *scanPad++ = block.L;
      *scanPad++ = block.R;
   } while( scanPad != endPad );

   return ans;
}



/// Iterator used to traverse data in all encryption routines below
typedef core::BlockIterator DIter;


/// Returns a new block made of the xor-ed fields of @a a and @a b.
inline
Block operator ^ ( Block const& a, Block const& b )
{ return Block( a.L ^ b.L , a.R ^ b.R ); }




void encrypt_ECB( Pad const& pad, void const* src, void* dst, size_t byteSize )
{
   DIter       srcScan(src);
   DIter const srcEnd (src, byteSize);
   DIter       dstScan(dst);
   for(  ; srcScan != srcEnd ; ++srcScan, ++dstScan )
      *dstScan = core::encipherBlock( pad, *srcScan );
}

void decrypt_ECB( Pad const& pad, void const* src, void* dst, size_t byteSize )
{
   DIter       srcScan(src);
   DIter const srcEnd (src, byteSize);
   DIter       dstScan(dst);
   for(  ; srcScan != srcEnd ; ++srcScan, ++dstScan )
		*dstScan = core::decipherBlock( pad, *srcScan );
}


void encrypt_CBC( Pad const& pad, void const* src, void* dst, size_t byteSize, Block* pChain )
{
   Block chain(0,0);
   if( pChain ) chain = *pChain;
   DIter       srcScan(src);
   DIter const srcEnd (src, byteSize);
   DIter       dstScan(dst);
   for(  ; srcScan != srcEnd ; ++srcScan, ++dstScan )
      *dstScan = chain = core::encipherBlock( pad, *srcScan ^ chain );
   if( pChain ) *pChain = chain;
}

void decrypt_CBC( Pad const& pad, void const* src, void* dst, size_t byteSize, Block* pChain )
{
   Block chain(0,0);
   if( pChain ) chain = *pChain;
   DIter       srcScan(src);
   DIter const srcEnd (src, byteSize);
   DIter       dstScan(dst);
   for(  ; srcScan != srcEnd ; ++srcScan, ++dstScan )
   {
		Block crypt = *srcScan;
		*dstScan = chain ^ core::decipherBlock( pad, crypt );
		chain = crypt;
	}
   if( pChain ) *pChain = chain;
}


void encrypt_CFB( Pad const& pad, void const* src, void* dst, size_t byteSize, Block* pChain )
{
   Block chain(0,0);
   if( pChain ) chain = *pChain;
   DIter       srcScan(src);
   DIter const srcEnd (src, byteSize);
   DIter       dstScan(dst);
   for(  ; srcScan != srcEnd ; ++srcScan, ++dstScan )
      *dstScan = chain = ( *srcScan ^ core::encipherBlock( pad, chain ) );
   if( pChain ) *pChain = chain;
}

void decrypt_CFB( Pad const& pad, void const* src, void* dst, size_t byteSize, Block* pChain )
{
   Block chain(0,0);
   if( pChain ) chain = *pChain;
   DIter       srcScan(src);
   DIter const srcEnd (src, byteSize);
   DIter       dstScan(dst);
   for(  ; srcScan != srcEnd ; ++srcScan, ++dstScan )
   {
		Block crypt = *srcScan;
		*dstScan = crypt ^ core::encipherBlock( pad, chain );
		chain = crypt;
	}
   if( pChain ) *pChain = chain;
}



}//namespace blowfish
