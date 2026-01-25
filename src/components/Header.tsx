import Link from "next/link";

export default function Header() {
  return (
    <header className=" flex justify-between items-center p-4">
      <Link href="/">
        <h1 className='text-blue-800 text-2xl sm:text-3xl md:text-4xl font-bold'>건강수명과의 30년 게임</h1>
      </Link>
      <nav className=' font-semibold gap-4 flex justify-between'>
        {/* <Link href="/dashboard">강의자료</Link> */}
        {/* <Link href="/report">보고서</Link> */}
        <Link href="/docu">글</Link>
        <Link href="/contact">연락처</Link>
        {/* <Link href="/reference">참고문헌</Link> */}
        <Link href="/aboutMe">나는?</Link>
      </nav>
    </header>
  );
}

