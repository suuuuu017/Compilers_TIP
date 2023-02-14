package tip.analysis

import tip.ast._
import tip.lattices._
import tip.ast.AstNodeData.DeclarationData
import tip.solvers._
import tip.cfg._
import tip.ast.AstOps._
import tip.cfg.CfgOps._

import scala.collection.immutable.Set

/**
 * Base class for live variables analysis.
 */
abstract class ReachingDefAnalysis(cfg: IntraproceduralProgramCfg) extends FlowSensitiveAnalysis(true) {

  val lattice: MapLattice[CfgNode, PowersetLattice[AAssignStmt]] = new MapLattice(new PowersetLattice())

  val domain: Set[CfgNode] = cfg.nodes

  NoPointers.assertContainsProgram(cfg.prog)
  NoRecords.assertContainsProgram(cfg.prog)

  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element =
    n match {
      case r: CfgStmtNode =>
        r.data match {
          case as: AAssignStmt =>
            as.left match {
              case ass: AIdentifier =>
                s.filter(x =>
                  x.left match {
                    case id: AIdentifier => id.name != ass.name
                    case _ => true
                  }) ++ Set(as);
              case _ => s
            }
          case _ => s
        }
      case _ => s
    }
}

/**
 * Live variables analysis that uses the simple fixpoint solver.
 */
class ReachingDefAnalysisSimpleSolver(cfg: IntraproceduralProgramCfg)
  extends ReachingDefAnalysis(cfg)
    with SimpleMapLatticeFixpointSolver[CfgNode]
    with ForwardDependencies

/**
 * Live variables analysis that uses the worklist solver.
 */
class ReachingDefAnalysisWorklistSolver(cfg: IntraproceduralProgramCfg)
  extends ReachingDefAnalysis(cfg)
    with SimpleWorklistFixpointSolver[CfgNode]
    with ForwardDependencies
