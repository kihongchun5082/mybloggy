import ContactForm from "@/components/ContactForm";
import { Metadata } from "next";
import { AiFillGithub, AiFillLinkedin, AiFillYoutube } from "react-icons/ai";

export const metadata: Metadata = {
  title: '한국형 책임의료조직 전기홍 블로그 연락하기',
  description: '한국형 책임의료조직 전기홍 블로그 메일 보내기'
}

const LINKS = [
  { icon: <AiFillGithub />, url:'' },
  { icon: <AiFillLinkedin />, url:'' },
  { icon: <AiFillYoutube />, url: '' },
]  
export default function ContactPage() {
  return (
    <section className=" flex flex-col items-center">
      <h2 className=" text-3xl font-bold my-3">연락하시려면?</h2>
      <p>blogforkoreaaco2024@kihongchun.com</p>
      <ul className=" flex gap-4 my-2">
        {LINKS.map((link, index) => (
          <a
            key={index}
            href={link.url}
            target="_blank"
            rel="noreferrer"
            className=" text-5xl hover:text-yellow-500">
            {link.icon}
          </a>
        ))}
      </ul>
      <h2 className=" text-2xl font-semibold mt-6 mb-2">
        이메일 보내시려면...
      </h2>
      <ContactForm />
    </section>
  );
}
// https://drive.google.com/file/d/1vPF6a42beQ3vm2YOm34VP_cY9-7dxXvh/view?usp=sharing
// https://drive.google.com/file/d/1vPF6a42beQ3vm2YOm34VP_cY9-7dxXvh/view?usp=drive_link
// https://drive.google.com/drive/folders/1xDg0D8qS534J3gA21ULNLNBuv1AWAwpZ?usp=drive_link
// https://contacts.google.com/blogforkoreaaco2024@kihongchun.com