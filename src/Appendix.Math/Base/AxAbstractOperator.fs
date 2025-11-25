namespace Appendix.Math.Base
///
/// Template of all mathematical operators defined after
/// 
/// The model "f('TObject) -> 'TResult" applies for all same
/// objects/classes and implemented derivative classes must be understandable
/// for others
/// 
/// Compare it with simplified speech: "'TQuestion -> 'TAnswer". No more details
/// 
[<AbstractClass>]
type AxAbstractOperator<'TObject, 'TResult>() =
    /// Abstract member must be implemented answering function
    abstract member get : 'TResult 
    /// Abstract member must be ToString implemented function
    /// <c>let per = new AxPermanent().Latex("A")</c> returns
    /// traditional fully described string of answer
    ///
    /// <c>"\text{Per}(A) = 59" </c>
    abstract member latex : string -> string
    /// Abstract member must be documented and returned 
    /// declaration of expected implementing operator
    /// 
    /// <c>perm: \mathbb{L} \longmapsto \mathbb{R}</c>
    /// <c>Per: \mathbb{L} \longmapsto \mathbb{R}</c>
    abstract member abstractDecl : string