import Hero from "@/components/Hero";
import { Metadata } from "next";
import Image from "next/image";

export const metadata: Metadata = {
  title: '나에 대해',
  description: '전기홍 소개'
}
export default function AboutMe() {
  const TITLE_CLASS = "text-2xl font-bold text-gray-800 my-2"
  return (
    <>
      <Hero />
      <section className=" bg-gray-100 shadow-lg p-8 m-8 text-center ">
        <h2 className={TITLE_CLASS}>나는 누구인가?</h2>
        <p>국민의 건강수명 연장을 바라는 예방의학자이고,<br/>
        장애와 사망에 이르게 하는 최종 결과 질환 발생을 구조적으로<br />
        최소화할 수 있는 국가 보건의료제공체계 구축을 위해 노력하고 있음
        </p>
        <h2 className={TITLE_CLASS}>주요 경력</h2>
        <p>아주대학교 의과대학 예방의학교실 교수(1990.9-2021.2)<br />
        건강보험심사평가원 평가위원(2016.3-2021.10)<br />
        우진산업보건연구원 의사(2021.11-현재)
        </p>
        <h2 className={TITLE_CLASS}>학위/전공</h2>
        <p>연세대학교 대학원 보건학과 보건학박사<br />
        전공: 보건의료관리, 국가보건의료체계, 만성질환관리, 지역사회보건의료
        </p>
      </section>
    </>
  )
  }