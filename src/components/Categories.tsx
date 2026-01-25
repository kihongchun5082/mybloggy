
type Props = {
  categories: string[],
  selected: string,
  onClick: (category: string) => void
}
export default function Categories({categories, selected, onClick}: Props) {
  return (
    <section className=" text-left p-4">
      <h2 className=" text-center text-lg font-bold border-b-2 border-sky-500 mb-2">
        {'구 분'}
      </h2>
      <ul>
        {categories.map((category) => (
          <li
            className={`cursor-pointer hover:text-red-900 ${
              category === selected && 'text-sky-600'
            }`}
            key={category}
            onClick={() => onClick(category)}>
            {category}
          </li>
        ))}
      </ul>
    </section>
  );
}

