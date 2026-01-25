import Image from "next/image";
import Link from "next/link";
import path from "path";
// import profileImage from "../../public/profile_noBorder.png" public 폴더의 것은 이것보다 src에 직접 쓰는게 더 좋음

export default function Hero() {
  return (
    <section className="text-center mx-auto bg-slate-400">
    {/* <section className='text-center p-2'> */}
      <Image
        // src={profileImage}
        src='/profile_noBorder.png'
        alt='전기홍 사진'
        className=" rounded-full mx-auto"
        width={250}
        height={250}
        priority
      />
      <h2 className=" text-2xl font-bold mt-2 mb-2">
        {'안녕하십니까? 전기홍입니다.'}
      </h2>
      <h3 className=" text_2xl font-semibold mb-2">
        {'예방의학을 전공했습니다. 보건정책과 관리가 전문분야입니다.'}
      </h3>
      <p className=" text-sm sm:text-base">저의 바람은 더 많은 사람들이 노년을 건강하고 행복하게 지내는겁니다.</p>
      <p className=" text-sm sm:text-base">어려운 주제지만 지속적으로 추구해야 할 가치라고 생각합니다.</p>
      {/* </div> */}
      <Link href='/contact'>
        {/* <button className="p-1 text-sm font-bold rounded-lg bg-yellow-500 border-violet-900 border-2">이멜 주소</button> */}
        <button className="font-bold rounded-xl bg-blue-500 py-1 px-4 mt-2">
          이멜 주소
        </button>
      </Link>
    </section>
  );
}
