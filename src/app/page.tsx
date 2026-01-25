import CarouselPosts from '@/components/CarouselPosts'
import Hero from '@/components/Hero'
import MyThoughts from '@/components/MyThoughts'

export default function HomePage() {
  return (
    <section className=' bg-slate-400'>
      <Hero />
      <MyThoughts />
      <CarouselPosts />
    </section>
  )
}
