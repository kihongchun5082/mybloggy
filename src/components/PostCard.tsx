import { Post } from "@/service/posts";
import Image from "next/image";
import Link from "next/link";
// import postImage from '/Users/kihongchun/Library/Mobile Documents/com~apple~CloudDocs/myBlog/public/images/posts/review-2023.png'

type Props = { post: Post }
export default function PostCard({
  post: { title, description, date, category, path },
}: Props) {
  // const iCloudPath = '/Users/kihongchun/Library/Mobile Documents/com~apple~CloudDocs/myBlog'
  return (
    <article className=" rounded-md bg-violet-200 overflow-hidden shadow-2xl hover:shadow-3xl">
      {/* <Link href={`/posts/${path}`}> */}
      <Image
        className=" w-full"
        src={`/images/posts/${path}.png`}
        alt={title}
        width={300}
        height={500}
      />
      {/* </Link> */}
      <div className=" flex flex-col items-center p-4">
        <time className=" font-semibold self-end text-blue-800">{date.toString()}</time>
        <h3 className=" text-lg font-bold">{title}</h3>
        <p className=" w-full truncate text-center">{description}</p>
        <span className=" text-sm rounded-lg bg-green-100 px-2 my-2">
          {category}
        </span>
      </div>
    </article>
  );
}

