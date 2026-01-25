import AdjacentPostCard from "@/components/AdjacentPostCard";
import PostContent from "@/components/PostContent";
import { getFeaturedPost, getPostData } from "@/service/posts";
import type { Metadata } from "next";
import Image from "next/image";

type PageProps = {
  params: Promise<{ slug:string }>
}
export async function generateMetadata({
  params }: PageProps): Promise<Metadata> {
  const { slug } = await params ;
  const { title, description } = await getPostData(slug);
  return {
    title,
    description,
  };
}

export default async function DocuPage({ params }: PageProps) {
      // 1. 전달된 slug에 해당하는 글 데이터를 읽어와서
      // 2. 데이터를 마크다운뷰에 표기하면 됨
  const { slug } = await params ;   
  const post = await getPostData(slug)
  const { title, path, next, prev } = post
      // console.log('post_slug;', post)
      // const iCloudPath = '/Users/kihongchun/Library/Mobile Documents/com~apple~CloudDocs/myBlog'
  return (
  <article className=" rounded-2xl overflow-hidden bg-gray-100 shadow-lg m-2">
    <Image className=" w-full h-1/5 max-h-125"
      src={`/images/posts/${path}.png`}
      alt={title}
      width={760}
      height={420}
    />
    <PostContent post={post} />
    <section className=" h-40 sm:h-48 md:h-56 flex shadow-md">
      {prev && <AdjacentPostCard post={prev} type='prev'/>}
      {next && <AdjacentPostCard post={next} type='next'/>}
    </section>
  </article>
  );
}

// export async function generateStaticsParams() {
//   const posts = await getFeaturedPost()
//   return posts.map((p) => ({
//     slug: p.path
//   }) )}

