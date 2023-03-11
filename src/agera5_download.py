import cdsapi

c = cdsapi.Client()
years = range(1979, 2023) #this years from 1979 to 2022

for year in years: 
    
    c.retrieve(
        'sis-agrometeorological-indicators',
        {
            'variable': 'precipitation_flux',
            'year': str(year),
            'month': [
            '01','02','03', '04', '05','06', '07', '08' 
            ], 
            #'month': ["09", "10", "11", "12"] #make a separate request for the remaining months by commenting on the one above
            'day': [
                '01', '02', '03',
                '04', '05', '06',
                '07', '08', '09',
                '10', '11', '12',
                '13', '14', '15',
                '16', '17', '18',
                '19', '20', '21',
                '22', '23', '24',
                '25', '26', '27',
                '28', '29', '30',
                '31',
            ],

            'area': [
                -9.25, 32.4, -18.25,
                36.5, #lat_max, lon_min, lat_min, lon_max
            ],
            'format': 'zip',
        },
    str(year)+'_jan_aug.zip') #change to str(year)+'_sep_dec.zip') when sending request for remaining months.
