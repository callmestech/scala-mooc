package homeworks.futures

import homeworks.HomeworksUtils.TaskSyntax

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */

  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */

  type PartitionedFuture[A] = Future[(List[A], List[Throwable])]

  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): PartitionedFuture[A] = {
    @tailrec
    def go(xs: List[Future[A]], acc: PartitionedFuture[A]): PartitionedFuture[A] = {

      def combine(fa: Future[A]): PartitionedFuture[A] =
        acc.flatMap { case (as, es) =>
          fa.map(a => (as :+ a, es)).recover(e => (as, es :+ e)) // can't use prepending because of test coupled to order of elements
        }

      xs match {
        case Nil     => acc
        case y :: ys => go(ys, combine(y))
      }
    }

    go(futures, Future.successful(List.empty[A] -> List.empty[Throwable]))
  }
}
