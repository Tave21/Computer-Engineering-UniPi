from multiprocessing.context import Process

from ingestion_system.ingestion_system_orchestrator import IngestionSystemOrchestrator
from preparation_system.PreparationSystemOrchestrator import PreparationSystemOrchestrator


def run_orchestrator():
    orchestrator = IngestionSystemOrchestrator()
    orchestrator.ingestion()

def run_preparation():
    orchestrator = PreparationSystemOrchestrator()
    orchestrator.run()


if __name__ == "__main__":
    ingestion_system = Process(target=run_orchestrator, args=())
    ingestion_system.start()
    preparation_system = Process(target=run_preparation, args=())
    preparation_system.start()
