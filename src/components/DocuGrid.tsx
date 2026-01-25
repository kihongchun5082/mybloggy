import Link from "next/link";
import { Post } from "../service/posts";
import PostCard from "./PostCard";

type Props = { posts: Post[]}
export default function DocuGrid({ posts }: Props) {
  console.log('posts;', posts)
  return (
    <ul className=" grid gap-5 grid-cols-1 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4 p-2">
      {posts.map((p) => (
        <li key={p.path}>
          <Link href={`/docu/${p.path}`}>
            <PostCard post={p} />
          </Link>
        </li>
      ))}
    </ul>
  );
}
