import pandas as pd
import numpy as np
from tqdm import tqdm
import pymc as pm
import warnings
import sqlite3
from matplotlib.backends.backend_pdf import PdfPages
warnings.filterwarnings("ignore")
import matplotlib.pyplot as plt

LIMITE = 22/24
ANFS = [83]
#data = pd.read_excel('data.xlsx')
data = pd.read_json('DISP4G.json')
data = data[[ 'Dia', 'ANF','Site','Disp' ]]
data = data[data['ANF'] == 83]
data['Dia'].max()

def bayesian_weibull_model(durations, events):
    with pm.Model() as model:
        lambda_ = pm.Gamma('lambda', alpha=2, beta=1)
        rho = pm.Gamma('rho', alpha=2, beta=1)
        
        def logp(event, duration):
            log_pdf = pm.math.log(rho) + (rho - 1)*pm.math.log(duration) - rho * pm.math.log(lambda_) - (duration / lambda_)**rho
            log_S = - (duration / lambda_)**rho
            return pm.math.switch(event, log_pdf, log_S)
        
        pm.Potential('likelihood', logp(events, durations))
    return model


data = data.drop(columns=['index'], errors='ignore')
data.columns = ['Data', 'ANF', 'Site', 'Disp']
data['Data'] = pd.to_datetime(data['Data'])
data['dia'] = data['Data'].dt.floor('D')

daily = (
    data
    .groupby(['Site', 'dia'])
    .agg(evento=('Disp', lambda x: int((x <= LIMITE).any())))
    .reset_index()
    .rename(columns={'Site': 'equipamento_id', 'dia': 'periodo'})
)
daily['periodo'] = pd.to_datetime(daily['periodo'])


probs_bayesian = {}
for site, df_site in tqdm(daily.groupby('equipamento_id'), desc='Processing sites'):
    try:
        df_site = df_site.sort_values('periodo')
        event_dates = df_site[df_site['evento'] == 1]['periodo']
        all_dates = df_site['periodo']
        
        durations = []
        events = []
        prev_date = None
        
        for date in event_dates:
            if prev_date is None:
                start_date = all_dates.min()
                duration = (date - start_date).days
            else:
                duration = (date - prev_date).days
            durations.append(duration)
            events.append(1)
            prev_date = date
        
        if prev_date is not None:
            end_date = all_dates.max()
            duration = (end_date - prev_date).days
            durations.append(duration)
            events.append(0)
        
        if len(durations) < 2:
            probs_bayesian[site] = np.nan
            continue
        
        durations = np.array(durations)
        events = np.array(events)
        
        model = bayesian_weibull_model(durations, events)
        with model:
            trace = pm.sample(1000, tune=500, chains=2, return_inferencedata=True, progressbar=False)
        
        s = durations[-1]
        lambda_samples = trace.posterior['lambda'].values.flatten()
        rho_samples = trace.posterior['rho'].values.flatten()
        
        S_s = np.exp(-(s / lambda_samples)**rho_samples)
        S_s_plus_1 = np.exp(-((s + 1) / lambda_samples)**rho_samples)
        prob_samples = (S_s - S_s_plus_1) / S_s
        
        probs_bayesian[site] = np.mean(prob_samples)
        
    except Exception as e:
        print(f"Erro no site {site}: {str(e)}")
        probs_bayesian[site] = np.nan

        # Convert probs_bayesian to a DataFrame
        probs_df = pd.DataFrame(list(probs_bayesian.items()), columns=['Site', 'Probability'])


probs_df=probs_df.sort_values(by='Probability', ascending=False)
probs_df.to_excel('probs_bayesian.xlsx', index=False)
filtered_probs_df = probs_df[probs_df['Probability'] > 0.05]

filtered_probs_df

# Save the filtered DataFrame to a new Excel file
# Plot the data for each site in filtered_probs_df
with PdfPages('site_disponibilidade_plots.pdf') as pdf:
    for site in filtered_probs_df['Site']:
        site_data = data[data['Site'] == site]
        plt.figure(figsize=(8, 4))
        plt.plot(site_data['Data'], site_data['Disp']* 100, label='Disponibilidade (%)')
        plt.axhline(y=LIMITE * 100, color='r', linestyle='--', label='Threshold (22/24)')
        plt.title(f'Disponibilidade Over Time for Site {site}')
        plt.xlabel('Date')
        plt.ylabel('Disponibilidade (%)')
        plt.legend()
        plt.grid()
        plt.tight_layout()
        pdf.savefig()  # Save the current figure into the PDF
        plt.close()  # Close the figure to free memory

# Plot the data for each site in filtered_probs_df
site = '4G-SJEPJ0'
site_data = data[data['Site'] == site]
plt.figure(figsize=(8, 4))
plt.plot(site_data['Data'], site_data['Disp'] * 100, label='Disponibilidade (%)')
plt.axhline(y=22/24 * 100, color='r', linestyle='--', label='Threshold (22/24)')
plt.title(f'Disponibilidade Over Time for Site {site}')
plt.xlabel('Date')
plt.ylabel('Disponibilidade (%)')
plt.legend()
plt.grid()
plt.tight_layout()
plt.show()


